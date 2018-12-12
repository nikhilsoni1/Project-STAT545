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



# operations----
root<-c("http://www.imdb.com",
        "http://www.wikipedia.com",
        "http://www.thehindu.com",
        "https://www.purdue.edu")
root1<-c("https://www.webmd.com/pets/cats/default.htm",
         "https://www.vanityfair.com/style/2018/08/dog-cloning-animal-sooam-hwang",
         "https://www.worldwildlife.org/species/tiger",
         "https://www.smithsonianmag.com/science-nature/the-truth-about-lions-11558237/")
store<-list()
store1<-list()
for(i in 1:length(root))
{
  print(root[i])
  store[[i]]<-sample_url(root[i])
}
for(i in 1:length(root1))
{
  print(root1[i])
  store1[[i]]<-sample_url(root1[i])
}

sample_url<-function(root)
{
  burn_in<-list()
  goto<-root
  for (i in 1:30)
  {
    burn_in[[i]]<-pager(root)
    print(i)
  }
  
  X_k<-burn_in[11:30]
  D_k<-unique(unlist(list_feature(X_k, "url")))
  D_k.prob<-list()
  D_k.MStep<-list()
  COUNTER<-1
  for(i in 1:length(D_k))
  {
    for(j in 1:10)
    {
      print(paste0(i,j))
      D_k.MStep[[COUNTER]]<-pager(D_k[i])$url
      COUNTER<-COUNTER+1
    }
  }
  D_k.MStep<-unlist(D_k.MStep)
  D_k.freq<-unclass(table(D_k.MStep))
  
  for(i in 1:length(D_k))
  {
    a<-D_k.freq[D_k[i]]/sum(D_k.freq)
    print(a)
    D_k.prob[[i]]<-D_k.freq[D_k[i]]/sum(D_k.freq)
  }
  D_k.prob<-unlist(D_k.prob)
  names(D_k.prob)<-D_k
  D_k.prob<-D_k.prob[!is.na(D_k.prob)]
  beta<-(0+min(D_k.prob))/2
  acc<-beta/D_k.prob
  U<-runif(length(acc))
  sample_store<-D_k.prob[U>acc]
  return(names(sample_store))
}
