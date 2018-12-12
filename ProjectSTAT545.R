save(list=ls(all=T), file="ProjectStat545.RData")
setwd("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/Project-STAT545")
load("ProjectStat545.RData")

# header----
install.packages("rvest")
library(rvest)
library(httr)

# functions----

list_feature<-function(lst, feature)
{
  store<-list()
  for(i in 1:length(lst))
  {
    store[[i]]<-lst[[i]][[feature]]
  }
  return(store)
}

jumper<-function(session, url)
{
  to<-tryCatch(
  {
    jump_to(session, url)
  },
  warning = function(w)
  {
    return(NULL)
  },
  error = function(e)
  {
    return(NULL)
  },
  finally =
  {
  })
  return(to)
}

href_parser<-function(session)
{
  result<-tryCatch(
    {
      html_attr(html_nodes(session, "a"), "href")
    },
    warning = function(w)
    {
      return(NULL)
    },
    error = function(e)
    {
      return(NULL)
    },
    finally =
    {
    })
  return(result)
}

pager<-function(root, seed.record=FALSE, seed.val=NULL)
{
  x<-sample(1:1000,1)
  if(seed.record)
  {
    set.seed(x)
  }
  if(!is.null(seed.val))
  {
    x<-seed.val
    set.seed(x)
  }
  webpage <- html_session(root)
  hrefs<-html_attr(html_nodes(webpage, "a"), "href")
  flag<-FALSE
  while(!flag)
  {
    target<-sample(hrefs, 1)
    to<-jumper(webpage, target)
    status<-to[["response"]][["status_code"]]
    to_hrefs<-href_parser(to)
    if(!is.null(to) && !is.null(to_hrefs))
    {
      if(status==200 & length(to_hrefs)>10)
      {
        flag<-TRUE
      }
    }
  }
  return(list(url=to[["url"]], seed=x, session=to))
}


MHalgo<-function(T.mat, X, perm, run=5000, verbose=F, B=2000)
{
  run<-run+B
  CTR<-1
  BURNT<-F
  MSG<-NULL
  lh<-list()
  perm.list<-list()
  while(CTR<=run)
  {
    if(BURNT)
    {
      acc<-acceptance(T.mat, X, perm)
      U<-runif(1)
      if(acc$acc != 1)
      {
        perm<-acc$new_perm
        MSG<-acc$new_msg
        lh<-c(lh, likelihood(T.mat, X, perm)$ll)
      }
      else
      {
        perm<-acc$old_perm
        MSG<-acc$old_msg
        lh<-c(lh, likelihood(T.mat, X, perm)$ll)
      }
      perm.list[[CTR-B]]<-perm
      if((CTR %% 100) == 0 && verbose)
      {
        print(CTR-B)
        print(paste(MSG[1:20], collapse = ''))
      }
    }
    else
    {
      acc<-acceptance(T.mat, X, perm)
      U<-runif(1)
      if(acc$acc !=1)
      {
        perm<-acc$new_perm
      }
      else
      {
        perm<-acc$old_perm
      }
    }
    if(CTR==B)
    {
      BURNT<-T
    }
    CTR<-CTR+1
  }
  lh<-unlist(lh)
  MSG<-paste(MSG, collapse = "")
  return(list(message=MSG, likelihood=lh, permutations=perm.list))
}
likelihood<-function(t)
{
  freq[freq<0]<-1e-03
  transition.matrix.result<-list(ll=sum(freq * log(t)))
  return(transition.matrix.result)
}
acceptance<-function(t)
{
  rand_perm<-random_perm(perm)
  a<-likelihood(T.mat, X, rand_perm)
  b<-likelihood(T.mat, X, perm)
  acc<-min(1, (a$ll/b$ll))
  return(list(new_perm=rand_perm, new_msg=a$msg, old_msg=b$msg, old_perm=perm, acc=acc))
}




# operations----
rm(i, burn_in1)
root<-"http://www.imdb.com"
burn_in1<-list()
goto<-root
cat("\014")
for (i in 1:1000)
{
  a<-pager(goto)
  burn_in1[[i]]<-a
  goto<-a$url
  print(i)
}

D_k<-burn_in[11:30]
D_k_sample<-sample(D_k, 1)
urls<-unique(unlist(list_feature(D_k, "url")))

new_page<-list()
for(i in 1:500)
{
  new_page[[i]]<-pager(sample(urls,1))
  print(i)
}

D_k<-burn_in[11:30]

freq <- unclass(table(unlist(list_feature(new_page, "url"))))
t<-freq/500
