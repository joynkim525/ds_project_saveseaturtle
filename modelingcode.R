finaldata <- read.csv(file.choose(),header = T)
finaldatareg <- read.csv(file.choose(),header=T)
finaldata2 <- read.csv(file.choose(),header = T)

view(finaldata)
view(finaldatareg)

colnames(finaldata2)
nrow(finaldata)
nrow(finaldatareg)

finaldata2[,3] <- finaldata2[,3] / (finaldata2[,22]+1)
finaldata2[,4] <- finaldata2[,4] / (finaldata2[,22]+1)
finaldata2[,5] <- finaldata2[,5] / (finaldata2[,22]+1)
finaldata2[,6] <- finaldata2[,6] / (finaldata2[,22]+1)
finaldata2[,7] <- finaldata2[,7] / (finaldata2[,22]+1)
finaldata2[,8] <- finaldata2[,8] / (finaldata2[,22]+1)
finaldata2[,10] <- finaldata2[,10] / (finaldata2[,22]+1)
finaldata2[,11] <- finaldata2[,11] / (finaldata2[,22]+1)
finaldata2[,14] <- finaldata2[,14] / (finaldata2[,22]+1)
finaldata2[,15] <- finaldata2[,15] / (finaldata2[,22]+1)
finaldata2[,16] <- finaldata2[,16] / (finaldata2[,22]+1)
finaldata2[,17] <- finaldata2[,17] / (finaldata2[,22]+1)
finaldata2[,18] <- finaldata2[,18] / (finaldata2[,22]+1)
finaldata2[,19] <- finaldata2[,19] / (finaldata2[,22]+1)
finaldata2[,20] <- finaldata2[,20] / (finaldata2[,22]+1)
finaldata2[,21] <- finaldata2[,21] / (finaldata2[,22]+1)

ggpairs(finaldata2[-c(1,2)])





library(ggfortify)
library(GGally)
colnames(finaldatareg)

ggpairs(finaldatareg[-c(1,2)])

lmmodel <- lm(data = finaldatareg, formula = 플라스틱~ 일인가구 + 비알콜음료업점포수 + 경제성장률 + 경제활동인구 + 지역내총생산 + 제조업 + 숙박및음식점업 + 취수시설)
summary(lmmodel)
autoplot(lmmodel)

ggpairs(log(finaldatareg[-c(1,2)]))

lmmodel <- lm(data = finaldatareg, formula = log(플라스틱) ~ log(일인가구) + log(비알콜음료업점포수) + 경제성장률 + log(경제활동인구) + log(지역내총생산) + log(제조업) + log(숙박및음식점업) + log(취수시설))

ggplot(finaldatareg,aes(x=일인가구 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=인구증가율 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=비알콜음료업점포수 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=경제성장률 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=경제활동인구 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=지역내총생산 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=제조업 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=숙박및음식점업 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldatareg,aes(x=취수시설 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)



lmmodel3 <- lm(data = finaldata2, formula = 플라스틱 ~ 일인가구 + 총쓰레기 + 비알콜음료업점포수 + 캔류 + 종이류 + 숙박및음식점업 + 플라스틱산업부가가치 + 농림어업 + 유리병류)
summary(lmmodel3)
autoplot(lmmodel3)

ggplot(finaldata2,aes(x=일인가구 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=총쓰레기 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=비알콜음료업점포수 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=캔류 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=종이류 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=숙박및음식점업 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=플라스틱산업부가가치 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=농림어업 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)
ggplot(finaldata2,aes(x=유리병류 ,y=플라스틱)) + geom_point() + geom_smooth(method="lm", se=FALSE)

ggpairs(finaldata2[,c(3,8,4,11,7,5,19,10,20,6)])

# Random forest
library(MASS)
library(randomForest)
library(caret)

set.seed(12345)
rffit <- randomForest(플라스틱 ~ 총쓰레기 + 종이류 + 유리병류 + 캔류 + 일인가구 + 인구증가율 + 플라스틱산업부가가치 + 비알콜음료업점포수 + 경제성장률 + 과대포장금지법 + 배달량 + 원유수입단가 + 경제활동인구 + 지역내총생산 + 제조업 + 숙박및음식점업 + 농림어업 + 취수시설, data = finaldata2, importance = TRUE)
importance(rffit)
summary(rffit)
varImpPlot(rffit, main = "varImpPlot of model")
