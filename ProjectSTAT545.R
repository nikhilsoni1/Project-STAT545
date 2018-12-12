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


MHalgo<-function(a,b)
{
  
  beta<-(0+min(a))
  print(beta/b)
  acc<-beta/b
  U<-runif(1)
  for(i in 1:395){
    if(U<acc)
    {
      random_sample[[i]]<-
    }
  }
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
}

D_k<-burn_in1[100:663]
D_k.urls<-unique(unlist(list_feature(D_k, "url")))
new_page<-list()
for(i in 1:500)
{
  new_page[[i]]<-pager(sample(D_k.urls,1))
  print(i)
}
freq<-unclass(table(unlist(list_feature(new_page, "url"))))
t<-freq/500

X<-burn_in1[100:663]
freq.X<-unclass(table(unlist(list_feature(X, "url"))))
t.x<-freq.X/length(X)
