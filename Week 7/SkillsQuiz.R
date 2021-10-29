library(tidyverse)
library(mosaic)
library(car)
library(pander)


str(KidsFeet)

#1
len.lm <- lm(length~width + I(width^2), data=KidsFeet)
summary(len.lm)
lb <- len.lm$coefficients

ggplot(KidsFeet, aes(x=width,y=length))+
  geom_point()+
  stat_function(fun=function(x) lb[1]+lb[2]*x+lb[3]*x^2)
#2
sex.lm <- lm(length~width + sex, data=KidsFeet)
summary(sex.lm)
sb <- sex.lm$coefficients

ggplot(KidsFeet, aes(x=width,y=length, color = sex))+
  geom_point()+
  stat_function(fun=function(x) (sb[1]+sb[3])+sb[2]*x, color="red")+
  stat_function(fun=function(x) (sb[1])+sb[2]*x)

#sex and quadratic
sq.lm <- lm(length~width + I(width^2) + sex + + I(width^2):sex, data=KidsFeet)
summary(sq.lm)
sqb <- sq.lm$coefficients

ggplot(KidsFeet, aes(x=width,y=length, color = sex))+
  geom_point()+
  stat_function(fun=function(x) (sqb[1]+sqb[4])+sqb[2]*x+(sqb[3]+sqb[5])*x^2, color="red")+
  stat_function(fun=function(x) (sqb[1])+sqb[2]*x+(sqb[3])*x^2)

#simple
feet.lm <- lm(length~width, data=KidsFeet)
summary(feet.lm)
fb <- feet.lm$coefficients

ggplot(KidsFeet, aes(x=width,y=length))+
  geom_point()+
  stat_function(fun=function(x) fb[1]+fb[2]*x)

#####Question 2

View(Loblolly)

ggplot(Loblolly,aes(x=age,y=height))+
  geom_point()+
  geom_smooth(method = "loess",se=F, color = "red")+
  geom_smooth(method = "lm", se=F)+
  stat_function(fun=function(x) b[1]+b[2]*x+b[3]*x^2)

lob.lm <- lm(height~age+I(age^2), data = Loblolly)
b <- lob.lm$coefficients
summary(lob.lm)


##### Question 3

?Utilities

pairs(Utilities, panel = panel.smooth)

util.lm <- lm(elecbill~kwh, data=Utilities)
summary(util.lm) %>% pander()

plot(util.lm, which = 1:2)

#adds the residuals and fitted values to pairs plot
cbind(R = util.lm$res, Fit = util.lm$fit, Utilities) %>% 
  pairs(panel = panel.smooth)

?cbind

util.lm <- lm(elecbill~year+I(month^2), data=Utilities)
summary(util.lm) %>% pander()

util.lm <- lm(elecbill~kwh+year+temp+I(temp^2), data=Utilities[-95,])
summary(util.lm) %>% pander()

plot(elecbill~kwh+year+temp+I(temp^2), data= Utilities)

boxCox(util.lm,xlim =c(.3,.8))

