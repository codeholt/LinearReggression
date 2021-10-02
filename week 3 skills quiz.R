library(tidyverse)
library(car)

View(Davis)
?Davis

davis.lm <- lm(height~weight,data=Davis)


ggplot(data=Davis, aes(x=weight,y=height))+
  geom_point()+
  geom_smooth(method='lm', se=F)


par(mfrow=c(1,3))
plot(davis.lm, which=1:2)
plot(davis.lm$residuals)


summary(davis.lm)
Davis2 <- Davis %>% 
  filter(weight != 166 & height != 57)
View(Davis2)

ggplot(data=Davis2, aes(x=weight,y=height))+
  geom_point()+
  geom_smooth(aes(color= "Outlier"),method='lm', se=F)+
  geom_smooth(data=Davis,aes(color= "without") method='lm', se=F, color = "darkgray")#+
#  scale_colour_manual( 
#                      breaks = c("Outlier", "without"),
#                      values = c("red", "green"))
  

davis2.lm <- lm(height~weight,data=Davis2)
summary(davis2.lm)

par(mfrow=c(1,3))
plot(davis2.lm, which=1:2)
plot(davis2.lm$residuals)


#######
#Question 2


View(Prestige)
?Prestige
prestige.lm <- lm(income~prestige,data=Prestige)

summary(prestige.lm)

ggplot(data=Prestige, aes(x=prestige,y=income))+
  geom_point()+
  geom_smooth(method='lm',se=F)
summary(prestige.lm)$sigma

par(mfrow=c(1,3))
plot(prestige.lm, which=1:2)
plot(prestige.lm$residuals)


######
#question 3

?Burt
burt.lm <- lm(IQbio~IQfoster,data = Burt)
View(Burt)
summary(burt.lm)

ggplot(data=Burt, aes(x=IQfoster,y=IQbio))+
  geom_point()+
  geom_smooth(method='lm',se=F)

par(mfrow=c(1,3))
plot(burt.lm, which=1:2)
plot(burt.lm$residuals)



#####
#question4

cars.lm <- lm(mpg~disp,data=mtcars)
summary(cars.lm)

ggplot(data=mtcars, aes(x=disp,y=mpg))+
  geom_point()+
  geom_smooth(method='lm',se=F)

par(mfrow=c(1,3))
plot(cars.lm, which=1:2)
plot(cars.lm$residuals)



###
#question 5

orange.lm <- lm(circumference~age, data= Orange)
summary(orange.lm)

ggplot(data=Orange, aes(x=age,y=circumference))+
  geom_point()+
  geom_smooth(method='lm',se=F)

par(mfrow=c(1,3))
plot(orange.lm, which=1:2)
plot(orange.lm$residuals)

mean(orange.lm$residuals^2) 
orange.lm$residuals

par(mfrow=c(1,1))
boxCox(orange.lm)
orangeTransf.lm <- lm(sqrt(circumference)~age, data= Orange)
summary(orangeTransf.lm)


b <- orangeTransf.lm$coefficients
ggplot(data=Orange, aes(x=age,y=circumference))+
  geom_point()+
  stat_function(fun=function(x) (b[1]+b[2]*x)**2)+
  geom_smooth(method='lm',se=F)


par(mfrow=c(1,3))
plot(orangeTransf.lm, which=1:2)
plot(orangeTransf.lm$residuals)

sqrt(predict(orange.lm, data.frame(age=c(500))))

