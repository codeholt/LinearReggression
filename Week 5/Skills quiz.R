library(tidyverse)
library(mosaic)
library(car)
library(alr4)




View(BGSall)
?BGSall

bgs.lm <- lm(HT18~HT2, data=BGSall)


summary(bgs.lm)

ggplot(data=BGSall, aes(x=HT2,y=HT18))+
  geom_point()+
  geom_smooth(method='lm')

2*pt(-abs(2.742), 134)


tvalSlope <- (1.4441-2)/(.1901)
2*pt(-abs(tvalSlope), 134)

confint(bgs.lm)

plot(bgs.lm, which = 1:2)
predict(bgs.lm, data.frame(HT2=83.82), interval = "predict")

(24+9)*2.54


?wblake
View(wblake)

wb.lm <- lm(Scale~Length, data=wblake)
summary(wb.lm)
plot(sqrt(Scale)~Length,data=wblake)


ggplot(data=wblake,aes(y=Scale,x=Length))+
  geom_point(color="darkgrey", alpha=.5)+
  geom_smooth(method="lm",color="darkgrey", alpha=.5, se=F)

plot(wb.lm,which = 1:2)

boxCox(wb.lm)

wbSqrt.lm <- lm(sqrt(Scale)~Length, data=wblake)
summary(wbSqrt.lm)

plot(wbSqrt.lm,which = 1:2)

predict(wb.lm,data.frame(Length=250), interval = 'predict')
predict(wbSqrt.lm,data.frame(Length=250), interval = 'predict')**2
