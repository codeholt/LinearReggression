library(tidyverse)
library(car)

carPrice <- read_csv("Cars Data (GLS VW passat).csv")
View(carPrice)
rows <- c(1:55,57:58)
carPrice <- carPrice[rows,] 
carPrice[-(23),]

#regression and diagnostics
price.lm <- lm(Price~Mileage, data=carPrice)
par(mfrow=c(1,3))
plot(price.lm, which=1:2)
plot(price.lm$residuals)
#data not linear, apply transformation
boxCox(price.lm)

priceLog.lm <- lm(log(Price)~Mileage, data=carPrice)
par(mfrow=c(1,3))
plot(priceLog.lm, which=1:2)
plot(priceLog.lm$residuals)



bLog <- priceLog.lm$coefficients
ggplot(data=carPrice, aes(x=Mileage,y=Price))+
  geom_point(color='steelblue')+
  #geom_smooth(method='lm', se=FALSE)+
  stat_function(fun=function(x) exp(bLog[1]+bLog[2]*x), color='black')+
  geom_point(aes(x=78200, y=9141), color="red", size=2)+
  geom_point(aes(x=200000,y=sell[2]), color = "red", size = 2)+
  labs(title="VW Passat Sale Value Per Mile Driven")+
  geom_text(aes(x=140000, y=10000, label = "Bought car at 78200 Miles"))+
  geom_text(aes(x=260000, y=sell[2]+1000, label = "sell car at 200000 Miles"))+
  theme_bw()

sell <- predict(priceLog.lm, data.frame(Mileage=c(91000, 200000))) %>% exp()
predict(priceLog.lm, data.frame(Mileage=c(78200, 150000))) %>% 
  exp()

#

mileageSeq <- c(78200, seq( 80000, 200000, by = 5000))

sale <- predict(priceLog.lm, data.frame(Mileage=mileageSeq)) %>% 
  exp()

driven <- mileageSeq-78200
purchaseCostPerMile <- driven/sale
purchaseCostPerMile

ggplot()+
  geom_line(aes(x=driven,y=sale))+
  geom_point(aes(x=driven,y=sale))

ggplot()+
  geom_line(aes(x=mileageSeq,y=sale))+
  geom_point(aes(x=mileageSeq,y=sale))
ggplot()+
  geom_line(aes(x=driven,y=((7886-sale)/(mileageSeq-91000))))+
  geom_point(aes(x=driven,y=(7886-sale)/(mileageSeq-91000)))

  