library(httr)
library(rvest)
library(urltools)
library(tidyverse)
library(stringr)
library(magrittr)
library(xtable)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#######2000years#####
tw = list()
for ( j in 2:9){
  title_list<-c()
  for (i in 1:30){
    title_list[i] <- paste('https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=200',j,'.01.01&de=200',j,'.12.31&docid=&nso=so:r,p:from200',j,'0101to200',j,'1231,a:all&mynews=1&cluster_rank=10&start=',i,'1&refresh_start=0', sep="")
  }

  a=list()
  for (i in 1:length(title_list)){
    res <- GET(url=title_list[i],
               user_agent(agent='Googlebot/2.1 (+http://www.google.com/bot.html)'))
    status_code(res)
    text <- read_html(x=res, encoding = "ko_KR.UTF-8") %>% 
      html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
      html_text()
    text <- gsub("[\r\n\t]","", text)
    text <- gsub("[[:punct:]]","", text)
    text <- gsub("[[:cntrl:]]","",text)
    text <-   str_split(text, pattern=" ")
    a[[i]]<-text
  }
  tw[[j]] <- table(unlist(a)) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    rename('keyword'='Var1', 'freq'='Freq')
}


#######2010years#####
for ( j in 10:17){
  title_list<-c()
  for (i in 1:30){
    title_list[i] <- paste('https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=20',j,'.01.01&de=20',j,'.12.31&docid=&nso=so:r,p:from20',j,'0101to20',j,'1231,a:all&mynews=1&cluster_rank=10&start=',i,'1&refresh_start=0', sep="")
  }
  
  a=list()
  for (i in 1:length(title_list)){
    res <- GET(url=title_list[i],
               user_agent(agent='Googlebot/2.1 (+http://www.google.com/bot.html)'))
    status_code(res)
    text <- read_html(x=res, encoding = "ko_KR.UTF-8") %>% 
      html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
      html_text()
    text <- gsub("[\r\n\t]","", text)
    text <- gsub("[[:punct:]]","", text)
    text <- gsub("[[:cntrl:]]","",text)
    text <-   str_split(text, pattern=" ")
    a[[i]]<-text
  }
  tw[[j]] <- table(unlist(a)) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    rename('keyword'='Var1', 'freq'='Freq')
}


#change to loop

#####wordclould2002###

wc.year <- function(k){
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste('wordcloud in year', k))
  wordcloud(words = tw[[k]]$keyword,
          freq = tw[[k]]$freq,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = .1,
          scale = c(3, 0.5),
          colors = brewer.pal(4, "Dark2"))

warnings()
}
wc.year(2)
wc.year(3)
wc.year(4)
wc.year(5)
wc.year(6)
wc.year(7)
wc.year(8)
wc.year(9)
wc.year(10)
wc.year(11)
wc.year(12)
wc.year(13)
wc.year(14)
wc.year(15)
wc.year(16)
wc.year(17)
