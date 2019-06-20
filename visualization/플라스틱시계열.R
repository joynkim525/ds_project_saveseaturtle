a = read.csv('C:/Users/DSL/Desktop/2019-1/DSS/project/데이터셋.csv')
a

library(tidyverse)


##전국

whole <- filter(a, 시도별 == '합계')
whole

#시계열일반
ggplot(data = whole, aes(x = year, y = 플라스틱))+
  geom_line(color = "#00AFBB", size = 2)


#------비율-----------

ggplot(data = whole, aes(x = year, y = 플라스틱/총쓰레기))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(title='' )+
  labs(title='총쓰레기 대비 플라스틱' )



#--------멀티플롯---
df <- whole %>%
  select(year, 플라스틱, 총쓰레기) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title='연도별 플라스틱 배출량 및 총 쓰레기 배출량' )

#------시도별------
library(dplyr)
bg <- group_by(a, 시도별)
bgs <- select(bg, 시도별, year, 플라스틱)
bgs1 <- filter(bgs, 시도별 != '합계')
ggplot(bgs1, aes(x = year, y = 플라스틱)) + 
  geom_line(aes(color = 시도별), size = 1) +
  labs(title='시도별 플라스틱 배출량' )
