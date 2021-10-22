library(tidyverse)
library(mosaic)
library(car)
library(modelr)

#1
View(airquality)

air.lm <- lm(Wind~Solar.R, data=airquality)

summary(air.lm)
3.515^2
mse(air.lm,data=airquality)

mean(air.lm$residuals^2)


#2
View(RailTrail)
trail.lm <- lm(volume~lowtemp, data=RailTrail)
summary(trail.lm)
ggplot(data=RailTrail, aes(x=lowtemp,y=volume))+
  geom_point()+
  geom_smooth(method='lm')


#3
airInv <- lm(Ozone^-1 ~ Temp, data=airquality)
airSqrt <- lm(sqrt(Ozone) ~ Temp, data=airquality)
airLog <- lm(log(Ozone) ~ Temp, data=airquality)
airOzone <- lm(Ozone ~ Temp, data=airquality)

summary(airInv)
summary(airSqrt)
summary(airLog)
summary(airOzone)

ggplot(airquality, aes(x=Temp, y=Ozone)) +
  geom_point()

#5

sum(c(-7.1,13,-7.4,4.1,-2.7)^2)


#6
?mtcars
cars.lm <- lm(dist~speed, data=cars)
summary(cars.lm)
pt(-abs((3.8-3.932)/0.4155), 48)

#8
carsSqrt.lm <- lm(sqrt(dist)~speed, data=cars)
predict(carsSqrt.lm, data.frame(speed=20), interval = "predict")^2

#9
mtcars.lm <- lm(log(mpg)~wt, data=mtcars)
?mtcars
boxCox(mtcars.lm)

exp(predict(mtcars.lm, data.frame(wt=4)))

#11
?KidsFeet

GirlsFeet <- KidsFeet %>% 
  filter(sex == "G")
GirlsFeet
girls.lm <- lm(width~length, data=GirlsFeet)
predict(girls.lm, data.frame(length=25), interval = 'predict')


#12
temp.lm <- lm(Wind~Temp,data=airquality)
summary(temp.lm)
?airquality

#14
cars.lm$residuals %>% order()

#15
(0-109.736)/374.404
2*pt(-abs((0-109.736)/374.404), 30)


#16
?ChickWeight
chick.lm <- lm(weight~Chick, data=ChickWeight)
plot(chick.lm, which=1:2)
plot(chick.lm$residuals)

#18
(1.875^2)*22

#20
lob.lm <- lm(height~age,data=Loblolly)
plot(height~age,data=Loblolly)
abline(lob.lm, lty=1,lwd=1)
plot(lob.lm)

#21
orange.lm <- lm(circumference~age,data=Orange)
plot(orange.lm,which=1:2)
plot(orange.lm$residuals)

#22
?mpg

View(mpg)

plot(hwy ~ cty, data = mpg)

mpg.lm <- lm(hwy ~ cty, data=mpg)
summary(mpg.lm)

#23
summary(cars.lm)

boxCox(cars.lm)






