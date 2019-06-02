a = read.csv('C:/Users/DSL/Desktop/2019-1/DSS/project/전국합계쓰레기.csv')
a

library(tidyverse)

#------시계열일반-----
ggplot(data = a, aes(x = year, y = 총플라스틱배출량))+
  geom_line(color = "#00AFBB", size = 2)


#------비율-----------

ggplot(data = a, aes(x = year, y = 총플라스틱배출량/총발생량))+
  geom_line(color = "#00AFBB", size = 2)  



#--------멀티플롯---
df <- a %>%
  select(year, 총플라스틱배출량, 총발생량) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title='연도별 플라스틱 배출량 및 총 쓰레기 배출량' )
