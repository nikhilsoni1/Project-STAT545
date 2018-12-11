save(list=ls(all=T), file="ProjectStat545.RData")
setwd("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/Project-STAT545")
load("ProjectStat545.RData")

# header----
install.packages("rvest")
library(rvest)
library(httr)

# functions----

pager<-function(root, seed.val=NULL)
{
  x<-sample(1:1000,1)
  set.seed(x)
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
    to<-jump_to(webpage, target)
    status<-to[["response"]][["status_code"]]
    to_hrefs<-html_attr(html_nodes(to, "a"), "href")
    if(status==200 & length(to_hrefs)>10)
    {
      print("Shizz")
      print(length(to_hrefs))
      flag<-TRUE
    }
  }
  return(list(url=to[["url"]], seed=x, session=to))
}

# operations---- 
root<-"http://www.imdb.com"
burn_in<-list()
goto<-root
cat("\014")
for (i in 1:30)
{
  a<-pager(goto)
  burn_in[[i]]<-a
  goto<-a$url
  print(goto)
}

D_k<-burn_in[11:30]

temp<-list()
for(i in 1:30){
  temp[[i]]<-burn_in[[i]][["url"]]
}
unique(unlist(temp))


