library(tidyverse)
library(car)
library(mosaic)
library(ResourceSelection)
library(MASS)


#question 2
#get validation R^2
?cars
View(cars)
Yhat <- -17+4.2*cars$speed
ybar <- mean(cars$dist) #Yi is given by Ynew from the new sample of data

# Compute SSTO
SSTO <- sum( (cars$dist - ybar)^2 )

SSE <- sum( (cars$dist - Yhat)^2 )

#unadjusted R^2
r2 <- 1-(SSE/SSTO)
r2

#question 3
#lowess curves (week 7)
View(airquality)
plot(dist~speed, data = cars)
#f gives weight percent. 0.2 = 20%
lines(lowess(cars$speed,cars$dist, f = .2))


#question 4
#increas of odds for every point increase
exp(1.1423)


#question 7
#robust Regression
mtRob.lm <- rlm(hp~wt, data = mtcars)
summary(mtRob.lm)
coef(mtRob.lm)

ggplot(data=mtcars, aes(x=wt,y=hp))+
  geom_point()+
  stat_function(fun=function(x) coef(mtRob.lm)[1]+coef(mtRob.lm)[2]*x)
plot(hp~wt, data = mtcars)
curve(coef(mtRob.lm)[1]+coef(mtRob.lm)[2]*x, add = T)


# question 8
#logistic regression

titanicMen <- TitanicSurvival %>% 
  filter(sex=="male") 

titanicMen.glm <-  glm(survived=="yes"~age, data = titanicMen,family = "binomial")
summary(titanicMen.glm)

titanicFe <- TitanicSurvival %>% 
  filter(sex=="female")

titanicFE.glm <- glm(survived=="yes"~age, data = titanicFe,family = "binomial")
summary(titanicFE.glm)

plot(survived=="yes"~age, data = TitanicSurvival, col = sex)

#question 9

(19408+(-2002))+(-.1926+(.1202))*40000

#question 12
car.lm <- lm(dist~speed, data = cars)
summary(car.lm)
#caclulate nimber of standard deviations out from alternative
#estimate - hypothesis / std error
tval <- (3.9324-4.2)/0.4155
#doubled because of two tailed
pt(-abs(tval), 48)*2


#question 13

air.lm <- lm(Temp~Month+I(Month^2), data = airquality)
summary(air.lm)


#question 14
plot(car.lm)
plot(dist~speed, data = cars)

car.lm$residuals[23]

#question 15
View(KidsFeet)
feet.lm <- lm(width~length, data = KidsFeet)
predict(feet.lm, data.frame(length = 25), interval = "confidence")


#question 18
View(RailTrail)
plot(volume~cloudcover, data = RailTrail)
trail.lm <- lm(volume~cloudcover, data = RailTrail)
abline(coef(trail.lm)[1], coef(trail.lm)[2])
predict(trail.lm, data.frame(cloudcover = 8))


#question 19
#manual SSE

#opt1 <- data.frame(errors = c(-7.1,13,-7.4,4.1,-2.7))
sum(c(-7.1,13,-7.4,4.1,-2.7)^2)
sum( c(-8.7, 11.6 ,-8.6, 3, -3.6)^2 )
sum( c(-19.6, 6.8 ,-7.3, 10.4, -9.9)^2 )
sum( c(-2.6, 15.8 ,-6.3, 3.4, -5.1)^2 )

#question 20
View(Marriage)
prevMar.glm <- glm(prevcount>0~age, data = Marriage, family = "binomial")
summary(prevMar.glm)
predict(prevMar.glm, data.frame(age = 27), type = "response")

#question 21
plot(time ~ age, data=TenMileRace, col=sex)
race.lm <- lm(time ~ age + sex, data = TenMileRace)
summary(race.lm)


#question 22
airtemp.lm <- lm(Ozone~Wind*Temp, data = airquality)
summary(airtemp.lm)
ab <- coef(airtemp.lm)
plot(Ozone~Wind, data = airquality)
Temp = 70
curve(ab[1]+ab[2]*x+ab[3]*Temp+ab[4]*Temp*x, add = T)



#question 25
mtcars.lm <- lm(log(mpg)~hp, data = mtcars)
boxCox((mtcars.lm))
plot(mpg~hp, data = mtcars)
predict(mtcars.lm, data.frame(hp = 350)) %>% exp()
