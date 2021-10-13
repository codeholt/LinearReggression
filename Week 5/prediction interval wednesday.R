library(tidyverse)
library(car)
library(mosaic)
library(modelr)

View(Utilities)

util.lm <- lm(gasbill~temp,data=Utilities)
sqrtUtil.lm <- lm(sqrt(sqrt(gasbill))~temp,data=Utilities)
summary(util.lm)
boxCox(util.lm)

b <- util.lm$coefficients
b.sqrt <- sqrtUtil.lm$coefficients

pinkPred <- predict(util.lm, data.frame(temp=30), interval = "predict")
transPred <- predict(sqrtUtil.lm, data.frame(temp=30), interval = "predict")**4

ggplot(data=Utilities, aes(x=temp,y=gasbill))+
  geom_point()+
  #to make the stat function work, get the coefficients from the transformed LM
  stat_function(fun=function(x) (b.sqrt[1]+b.sqrt[2]*x)**4, color = 'skyblue')+
  geom_smooth(method="lm", se=F, color="hotpink")+
  geom_segment(aes(x=30,xend=30,y=transPred[2],yend=transPred[3]),
               size=4, color='skyblue', alpha=.01)+
  geom_segment(aes(x=30,xend=30,y=pinkPred[2],yend=pinkPred[3]), 
               size=4, color='hotpink', alpha=.01)+
  theme_bw()

#calculate the pvalue from the t value
2*pt(-abs(35.82),115)


plot(util.lm, which=1)
plot(util.lm, which=2)
plot(util.lm$residuals)
