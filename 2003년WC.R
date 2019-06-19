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

title_list <- c('https://search.naver.com/search.naver?where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_srt&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so%3Ar%2Cp%3Afrom20020101to20021231%2Ca%3Aall&mynews=1&refresh_start=0&related=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=14&start=11&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=50&start=21&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=60&start=31&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=70&start=41&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=80&start=51&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=90&start=61&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=101&start=71&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2003.01.01&de=2003.12.31&docid=&nso=so:r,p:from20020101to20021231,a:all&mynews=1&cluster_rank=111&start=81&refresh_start=0'
)
res1 <- GET(url= title_list[1],
            user_agent(agent='Googlebot/2.1 (+http://www.google.com/bot.html)'))
status_code(res1)
text1 <- read_html(x=res1, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text() 
text1 <- gsub("[\r\n\t]","", text1)
text1 <- gsub("[[:punct:]]","", text1)
text1 <- gsub("[[:cntrl:]]","",text1)
text1 <-   str_split(text1, pattern=" ")
tw1 <- table(unlist(text1)) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')
tw1 

res2 <- GET(url=title_list[2])
status_code(res2)
text2 <- read_html(x=res2, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text2 <- gsub("[\r\n\t]","", text2)
text2 <- gsub("[[:punct:]]","", text2)
text2 <- gsub("[[:cntrl:]]","",text2)
text2 <-   str_split(text2, pattern=" ")
tw2 <- table(unlist(text2)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res3 <- GET(url=title_list[3])
status_code(res3)
text3 <- read_html(x=res3, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text3 <- gsub("[\r\n\t]","", text3)
text3 <- gsub("[[:punct:]]","", text3)
text3 <- gsub("[[:cntrl:]]","",text3)
text3 <-   str_split(text3, pattern=" ")
tw3 <- table(unlist(text3)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res4 <- GET(url=title_list[4])
status_code(res4)
text4 <- read_html(x=res4, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text4 <- gsub("[\r\n\t]","", text4)
text4 <- gsub("[[:punct:]]","", text4)
text4 <- gsub("[[:cntrl:]]","", text4)
text4 <-   str_split(text4, pattern=" ")
tw4 <- table(unlist(text4)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res5 <- GET(url=title_list[5])
status_code(res5)
text5 <- read_html(x=res5, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text5 <- gsub("[\r\n\t]","", text5)
text5 <- gsub("[[:punct:]]","", text5)
text5 <- gsub("[[:cntrl:]]","",text5)
text5 <-   str_split(text5, pattern=" ")
tw5 <- table(unlist(text5)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res6 <- GET(url=title_list[6])
status_code(res6)
text6 <- read_html(x=res5, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text6 <- gsub("[\r\n\t]","", text6)
text6 <- gsub("[[:punct:]]","", text6)
text6 <- gsub("[[:cntrl:]]","",text6)
text6 <-   str_split(text6, pattern=" ")
tw6 <- table(unlist(text6)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res7 <- GET(url=title_list[7])
status_code(res7)
text7 <- read_html(x=res7, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text7 <- gsub("[\r\n\t]","", text7)
text7 <- gsub("[[:punct:]]","", text7)
text7 <- gsub("[[:cntrl:]]","",text7)
text7 <-   str_split(text7, pattern=" ")
tw7 <- table(unlist(text7)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res8 <- GET(url=title_list[8])
status_code(res8)
text8 <- read_html(x=res8, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text8 <- gsub("[\r\n\t]","", text8)
text8 <- gsub("[[:punct:]]","", text8)
text8 <- gsub("[[:cntrl:]]","",text8)
text8 <-   str_split(text8, pattern=" ")
tw8 <- table(unlist(text8)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')

res9 <- GET(url=title_list[9])
status_code(res9)
text9 <- read_html(x=res9, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@class="type01"]/li/dl/dt/a') %>% 
  html_text()
text9 <- gsub("[\r\n\t]","", text9)
text9 <- gsub("[[:punct:]]","", text9)
text9 <- gsub("[[:cntrl:]]","",text9)
text9 <-   str_split(text9, pattern=" ")
tw9 <- table(unlist(text9)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
  rename('keyword'='Var1', 'freq'='Freq')


tw <- rbind(tw1, tw2, tw3, tw4, tw5, tw6, tw7, tw8, tw9)
#change to loop

#####wordclould###


wordcloud(words = tw$keyword,
          freq = tw$freq,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = .1,
          scale = c(2, 0.3),
          colors = brewer.pal(4, "Dark2"),
          main='2003년')

warnings()


