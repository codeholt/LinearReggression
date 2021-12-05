library(tidyverse)
library(mosaic)
library(car)
library(ResourceSelection)

train <- read.csv("Math325Grades_Train-1.csv", stringsAsFactors = TRUE)
test <- read.csv("Math325Grades_Test-1.csv", stringsAsFactors = TRUE)

View(test)
View(train)
?train

train1 <- train %>% 
  mutate(FinalGrade = case_when(FinalGrade == "A" ~ 1, FinalGrade == "Other"~0))



train1 %>% 
  mutate(FinalGrade = case_when(FinalGrade == "A" ~ 1, FinalGrade == "Other"~0)) %>% 
  select(c(FinalGrade,
           ProjectTotal,
           AnalysisTotal,
           CritiqueTotal,
           SkillsQuizzesTotal,
           ClassActivitiesTotal,
           Math325NotebookOrganization,
           AssessmentQuizCompletionTotal,
           FinalExam)) %>% 
  pairs(panel = panel.smooth)


#checking certain parameters as they appear in the data
proj <- glm(FinalGrade~ProjectTotal, data = train, family = "binomial")#AIC = 127.87
summary(proj)

glm(FinalGrade~AnalysisTotal, data = train, family = "binomial") %>% summary()#AIC = 66.879

glm(FinalGrade~CritiqueTotal, data = train, family = "binomial") %>% summary()#AIC = 117.16

glm(FinalGrade~SkillsQuizzesTotal, data = train, family = "binomial") %>% summary()#AIC = 151.74

glm(FinalGrade~ClassActivitiesTotal, data = train, family = "binomial") %>% summary()#AIC = 150.4

glm(FinalGrade~Math325NotebookOrganization, data = train, family = "binomial") %>% summary()#AIC = 136.8

glm(FinalGrade~AssessmentQuizCompletionTotal, data = train, family = "binomial") %>% summary()#AIC = 150.34
#also had 0 or 1 probabilities

glm(FinalGrade~FinalExam, data = train, family = "binomial") %>% summary()#AIC = 130.02


#analysis seem to be the most significant with best AIC, starting there

#critique total not significant, nor is interaction
glm(FinalGrade~AnalysisTotal+CritiqueTotal, data = train, family = "binomial") %>% summary()#AIC = 66.473

#removes significance with interaction. not significant on own either
glm(FinalGrade~AnalysisTotal+ProjectTotal, data = train, family = "binomial") %>% summary()#AIC = 67.238

#skills quiz significant on .1, but not .05
glm(FinalGrade~AnalysisTotal+SkillsQuizzesTotal, data = train, family = "binomial") %>% summary()#AIC = 66.106

#All significant, but not on interaction
glm(FinalGrade~AnalysisTotal+FinalExam, data = train, family = "binomial") %>% summary()#AIC = 50.231

#significant on individual, not on interaction
glm(FinalGrade~AnalysisTotal+Math325NotebookOrganization, data = train, family = "binomial") %>% summary()#AIC = 60.867

#significant on individual, not on interaction
glm(FinalGrade~AnalysisTotal+ClassActivitiesTotal, data = train, family = "binomial") %>% summary()#AIC = 60.845


#ones to keep: Notebook organization, class activites, final exam, skills quiz, analysis total
#Final Model
myglm <- glm(FinalGrade~AnalysisTotal+FinalExam+ClassActivitiesTotal, data = train1, family = "binomial")#AIC = 44.679
summary(myglm)

hoslem.test(myglm$y, myglm$fitted.values)


b <- coef(myglm)
b

summary(train$AnalysisTotal)
summary(train$FinalExam)
summary(train$ClassActivitiesTotal)

train1 %>% 
  ggplot(aes(color =AnalysisTotal,y=FinalGrade,  x=FinalExam))+
  geom_smooth(method = "glm", 
              method.args = list(family="binomial"), formula = y~x, color = "darkgreen")+
  geom_jitter(height = .01)+
  theme_bw()
  



##Model Validation
#splts data into test and train
set.seed(123)
n <- nrow(train1)
keep <- sample(1:n, 80)
my_train <- train1[keep,]#40 values here
my_test <- train1[-keep,]# rest go here

#creates model on training and runs prediction
train.glm <- glm(FinalGrade~AnalysisTotal+FinalExam+ClassActivitiesTotal, data = my_train, family = "binomial")
myPreds <- predict(train.glm, newdata = my_test, type = "response")

#Correct vs incorrect guesses. False:0 = correctly guessed not getting an A, TRUE:1 Correctly guess getting an A
table(myPreds > .8, my_test$FinalGrade)
(19+18)/40


#plots the analysis total against final grade
train1 %>% 
  ggplot(aes(x=AnalysisTotal,y=FinalGrade))+
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family="binomial"), formula = y~x)

train1 %>% 
  ggplot(aes(x=ClassActivitiesTotal,y=FinalGrade))+
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family="binomial"), formula = y~x)


## Calculating predicted scores



test$FinalGrade <- predict(myglm, newdata = test, type = "response") >.8
#gradePred <- case_when(testPreds > .8 ~ "A", TRUE~0)

#test %>% 
#  mutate(FinalGrade = as.character(FinalGrade),
#    FinalGrade = as.character(case_when(as.numeric(FinalGradeOdds) > 0.8 ~ "A",
                                as.numeric(FinalGradeOdds) <= 0.8 ~  "Other")))

write.csv(test,"Math325Grades_Test-1.csv")


