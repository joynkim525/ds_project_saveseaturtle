library(httr)
library(rvest)
library(urltools)
library(tidyverse)
library(stringr)
library(magrittr)
library(xtable)


title_list <- c('https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=44&start=1&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=30&start=11&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=74&start=21&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=90&start=31&refresh_start=0',
                'https://search.naver.com/search.naver?&where=news&query=%ED%94%8C%EB%9D%BC%EC%8A%A4%ED%8B%B1&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=107&start=41&refresh_start=0')
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

tw <- rbind(tw1, tw2, tw3, tw4, tw5)
#change to loop

#####
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

wordcloud(words = tw$keyword,
          freq = tw$freq,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = .1,
          scale = c(2, 0.3),
          colors = brewer.pal(4, "Dark2"))

warnings()

#####
req <- GET(url='http://star.ohmynews.com/NWS_Web/OhmyStar/at_pg.aspx?CNTN_CD=A0002530609&CMPT_CD=P0010&utm_source=naver&utm_medium=newsearch&utm_campaign=naver_news')
status_code(req)
text <- read_html(x=req, encoding = "ko_KR.UTF-8") %>% 
  html_nodes(xpath='//*[@itemprop="articleBody"]') %>% 
  html_text() 

text
text <- gsub("[\r\n\t]","", text)
text <- gsub("[[:punct:]]","", text)
text <- gsub("[[:cntrl:]]","",text)
text <- str_split(text, pattern=" ")
summary(text)



