library(tidyverse)
library(mosaic)
library(car)
library(ResourceSelection)

?KidsFeet
KidsFeet88 <- KidsFeet %>% 
  mutate(year88 = as.numeric(birthyear == 88))
View(KidsFeet88)

month.glm <- glm(year88~birthmonth, data= KidsFeet88, family = "binomial")#AIC = 20.921
summary(month.glm)
bm <- coef(month.glm)

width.glm <- glm(year88~width, data= KidsFeet88, family = "binomial")#AIC = 37.877
summary(width.glm)

#predicting odds of birth year being 88 if born in may
monthPred <- predict(month.glm, newdata = data.frame(birthmonth = 5), type = "response")
monthPred6 <- predict(month.glm, newdata = data.frame(birthmonth = 6), type = "response")

monthPred-monthPred6
1-exp(bm[2])


##2

?RailTrail
boxplot(volume ~ precip>0, data=RailTrail, col="skyblue", 
        xlab="90 Days of Records from April to November", 
        ylab="Total Number of Users on the Trail that Day",
        main="Rain Seems to Reduce the Number of Users", 
        names = c("Days with No Rain", "Rainy Days"))


RT <- RailTrail %>% 
  mutate(precipOccurred = (precip > 0)) %>%
  select(precipOccurred, lowtemp, spring, summer, fall, cloudcover, weekday)

pairs(RT, panel = panel.smooth)

low.glm <- glm(precipOccurred~lowtemp, data = RT, family = "binomial")#AIC = 114.09
summary(low.glm)

cloud.glm <- glm(precipOccurred~cloudcover, data = RT, family = "binomial")#AIC = 82.098
summary(cloud.glm)

fall.glm <- glm(precipOccurred~fall, data = RT, family = "binomial")#AIC = 116.79
summary(fall.glm)



glm(precipOccurred~fall + lowtemp, data = RT, family = "binomial") %>% 
  summary()#AIC = 116.07

glm(precipOccurred~fall + cloudcover, data = RT, family = "binomial") %>% 
  summary()#AIC = 83.89

glm(precipOccurred~cloudcover + lowtemp, data = RT, family = "binomial") %>% 
  summary()#AIC = 84.096

glm(precipOccurred~spring + lowtemp, data = RT, family = "binomial") %>% 
  summary()#AIC = 115.44

glm(precipOccurred~summer + lowtemp, data = RT, family = "binomial") %>% 
  summary()#AIC = 114.49

glm(precipOccurred~weekday + lowtemp, data = RT, family = "binomial") %>% 
  summary()#AIC = 116.09

glm(precipOccurred~cloudcover + spring, data = RT, family = "binomial") %>% 
  summary()#AIC = 83.691

glm(precipOccurred~cloudcover + summer, data = RT, family = "binomial") %>% 
  summary()#AIC = 83.962

glm(precipOccurred~cloudcover + weekday, data = RT, family = "binomial") %>% 
  summary()#AIC = 82.753

