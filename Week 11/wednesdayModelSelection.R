library(tidyverse)
library(mosaic)
library(car)



data <- read.csv("Math425PastGrades-1.csv", stringsAsFactors = T)

m425 <- data %>% 
  mutate(Exam70 = as.numeric(Final.Exam>70),
         SampDist = as.numeric(as.character(Theory.Assignment...Sampling.Distributions.Unveiled)))

View(data)
pairs(m425, panel = panel.smooth)

#Cannot Compare AIC between y variables
#Lower AIC is better
#Akaike an Information Criterion
glm(Exam70~Math.425.Midterm, data = m425, family = "binomial") %>% summary()
glm(Exam70~MagicTwoGroups, data = m425, family = "binomial") %>% summary()
glm(Exam70~SampDist+I(SampDist^2), data = m425, family = "binomial") %>% summary()

glm(Exam70~Math.425.Midterm+I(Math.425.Midterm^2)+MagicTwoGroups, data = m425, family = "binomial") %>% 
  summary()

glm(Exam70~Math.425.Midterm*AttendedAlmostAlways+MagicTwoGroups, data = m425, family = "binomial") %>% 
  summary()#AIC 22.098

m425 %>% 
  ggplot(aes(x=Math.425.Midterm, y = Exam70, color = AttendedAlmostAlways))+
  geom_jitter(width = 0, height = .01)+
  geom_smooth(method = "glm", 
              method.args = list(family="binomial"), formula = y~x)+
  facet_wrap(~ MagicTwoGroups)

#checking goodnes of fit
library(ResourceSelection)

bob <- glm(Exam70~Math.425.Midterm, data = m425, family = "binomial") %>% summary()

sally <- glm(Exam70~Math.425.Midterm+MagicTwoGroups, data = m425, family = "binomial") %>% 
  summary()

#Null is model being good, alternative is model not good
#Pvalue high is model not rejected
hoslem.test(bob$y, bob$fitted.values)
predict(sally, data.frame(Math.425.Midterm = 60, MagicTwoGroups = 2), type = "response")

#validation

#predicted will be probabilities, but output is 0 or 1 ( above 70 on final or not)
set.seed(123)
n <- nrow(m425)
keep <- sample(1:n, 40)
my_train <- m425[keep,]#40 values here
my_test <- m425[-keep,]# rest go here


sallyTrain <- glm(Exam70~Math.425.Midterm+MagicTwoGroups, data = my_train, family = "binomial")
myPreds <- predict(sallyTrain, newdata = my_test, type = "response")


table(myPreds > 0.5, my_test$Exam70)
table(myPreds > 0.5)

#Number of people we guessed correctly / total observations
(11+7)/19
  
  
  
  