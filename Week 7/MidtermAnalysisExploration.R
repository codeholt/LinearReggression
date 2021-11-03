library(tidyverse)
library(mosaic)
library(car)

midata <- read.csv("Math425PastGrades.csv", stringsAsFactors = TRUE)
View(midata)



#checking varable interaction pairs
pairs(midata)
pairs(midata,panel = panel.smooth)
#didnt work, so Im going to convert the Gender, Attendance, Office Hours, class activities, and skills quiz to 1 or 0

#if I dont put the , stringsAsFactors = TRUE into the read.csv, this is needed for the pairs plot
midata2 <- midata %>% 
  mutate(Gender = case_when(Gender == "M" ~ 0,
                            Gender == "F" ~ 1),
         #changes attendance to Y=1 and N=0
         AttendedAlmostAlways = case_when(AttendedAlmostAlways == "Y"~1,
                                          AttendedAlmostAlways == "N"~0),
         SpentTimeInOfficeHours = case_when(SpentTimeInOfficeHours == "Y"~1,
                                            SpentTimeInOfficeHours == "N"~0),
         ClassActivitiesCompletedPerfectly = case_when(ClassActivitiesCompletedPerfectly == "Y"~1,
                                                       ClassActivitiesCompletedPerfectly == "N"~0),
         SkillsQuizzesCompletedPerfectly = case_when(SkillsQuizzesCompletedPerfectly == "Y"~1,
                                                     SkillsQuizzesCompletedPerfectly == "N"~0),
         #sections Winter 2020 = 0, Fall 2019 = 1
         Section = case_when(Section == "Winter 2020"~ 0,
                             Section == "Fall 2019"~ 1),
         #Making these columns numeric and changing EX values to NA
         Analysis...Predicting.the.Weather = as.numeric(Analysis...Predicting.the.Weather),
         Analysis...Car.Selling.Price = as.numeric(Analysis...Car.Selling.Price),
         Theory.Assignment...Residuals..Sums.of.Squares..and.R.Squared = as.numeric(Theory.Assignment...Residuals..Sums.of.Squares..and.R.Squared),
         Theory.Assignment...Sampling.Distributions.Unveiled = as.numeric(Theory.Assignment...Sampling.Distributions.Unveiled))
#averages the analysis grades into one score
midata2 <- midata2 %>% 
  mutate(meanAnalysis = (Analysis...Predicting.the.Weather + Analysis...Car.Selling.Price + 
                           Theory.Assignment...Residuals..Sums.of.Squares..and.R.Squared + Theory.Assignment...Sampling.Distributions.Unveiled)/4)

head(midata2)
View(midata2)
#checks the data types of each column
str(midata2[,-3])

#dropped the section column
pairs(midata2[,-3], panel = panel.smooth)

#Columns to maybe disclude from the model
#Section, time in office hours

#making a plot to view midterm, final, and total exam scores
ggplot(data=midata2)+
  geom_point(aes(x=Math.425.Midterm, y=(.7*Final.Exam + .3*Math.425.Midterm)))+
  geom_point(aes(x=Math.425.Midterm, y=Final.Exam), color = "red")

#positive keeps midterm
#negative better to drop
#put my predicted score on here with interval for final decision
#TODO make it look nice
ggplot(data=midata2)+
  geom_point(aes(x=Math.425.Midterm, y=(.7*Final.Exam + .3*Math.425.Midterm)-Final.Exam, color=as.factor(MagicTwoGroups)))+
  labs(x="Midterm Score (%)", y="Difference in Exam total if midterm is dropped ((Final+Midterm)-Final)")+
  geom_hline(yintercept = 0)


#step 1 Get model to predict final exam score
#for typing out all the varable names because they long
midata2 %>% 
  lm(Final.Exam~Math.425.Midterm + AttendedAlmostAlways + Assessment.Quizzes.Final.Score + MagicTwoGroups)

#actual LM code
mylm <- lm(Final.Exam~Math.425.Midterm + MagicTwoGroups, data= midata2)
summary(mylm)

#things to try
#Final.exam: Predicting the weather, Assesment quizsz final, class activities and magic 2
#midterm: Assesment quizs, 
#Gender: Time in Office
#Time in Office: Predicting the weather, Assesment Quiz
#residuals theory: car selling price, skills quiz, 
#Magic 2 groups: Final Quizes



#comparing midterm to final, color groups magic 2
ggplot(data=midata2,aes(x=Math.425.Midterm, y=Final.Exam, color=as.factor(MagicTwoGroups)))+
  geom_point()+
  labs(x="Midterm Score (%)", y="Final")+
  geom_smooth(method = 'lm')
  

######Notes
#simple linear regression has the midterm be significant, but the R^2 is low. using the pairs I can probably do better
#Attendance and gender look like they may have influence. little interaction though and its linear
#Gender wasnt significant. neither was almost always attended on its own
#picking up later. made good progress and a plan

#having midterm alone intercept wasnt significant
#attended almost always wasnt significant on its own, but interacts and makes midterm and intercept significant. itself significant at .1
#R^2 of .3984 with lm(Final.Exam~Math.425.Midterm + Math.425.Midterm:Assessment.Quizzes.Final.Score +  Math.425.Midterm:AttendedAlmostAlways, data= midata2)
#will need to ask what variables we can use and if we know them
#magic 2 seems to be the lynch pin. what is it, and what would mine be?
#magic2groups
# group 2 is hunch of strong math background
# group 1 is hunch on weak math background
# plan to compensate: make predictions from both.

#Step 2 calculate prediction intervals for final exam score 100% final or 70% final 30% midterm
pred <- predict(mylm, data.frame(Math.425.Midterm = c(64,64), MagicTwoGroups = c(1,2)), interval = 'predict')
predict(mylm, data.frame(Math.425.Midterm = 64, MagicTwoGroups = 1, interval = 'predict'))

ggplot(data=midata2,aes(x=Math.425.Midterm, y=Final.Exam, color=as.factor(MagicTwoGroups)))+
  geom_point()+
  scale_color_manual(values = c("steelblue", "firebrick"),
                     name="MagicTwoGroups")+
  labs(x="Midterm Score (%)", y="Final")+
  geom_smooth(method = 'lm')+
  #magic group 1
  geom_segment(aes(x=64, xend=64, y=pred[4],yend=pred[6]),size = 4, alpha = .01)+
  geom_point(aes(x=64, y=pred[1]),size = 3, color = 'black')+
  #magic group 2
  geom_segment(aes(x=64, xend=64, y=pred[3],yend=pred[5]),size = 4, alpha = .01, color = "steelblue")+
  geom_point(aes(x=64, y=pred[2]),size = 3, color = 'black')+
  theme_bw()+
  geom_vline(xintercept = 64)


#compare grade predictions
ggplot(data=midata2)+
  scale_color_manual(values = c("steelblue", "firebrick"),
                     name="MagicTwoGroups")+
  geom_point(aes(x=Math.425.Midterm, y=(.7*Final.Exam + .3*Math.425.Midterm)-Final.Exam, color=as.factor(MagicTwoGroups)))+
  #group 1
  geom_point(aes(x=64, y=(.7*pred[1] + .3*64)-pred[1]), color = "steelblue", size = 4)+
  geom_segment(aes(x=64, xend=64, y=(.7*pred[3] + .3*64)-pred[3],yend=(.7*pred[5] + .3*64)-pred[5]),size = 4, alpha = .01, color = "steelblue")+
  #group 2
  geom_point(aes(x=64, y=(.7*pred[2] + .3*64)-pred[2]), color = "firebrick", size = 4)+
  geom_segment(aes(x=64, xend=64, y=(.7*pred[4] + .3*64)-pred[4],yend=(.7*pred[6] + .3*64)-pred[6]),size = 4, alpha = .01, color = "firebrick")+
  labs(x="Midterm Score (%)", y="Difference in Exam total if midterm is dropped ((Final+Midterm)-Final)",title = "Final Exam grade Difference")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  annotate(x=50,y=-5, label="Better to drop midterm", geom="text")+
  annotate(x=50,y=10, label="Better to keep midterm", geom="text")
#Step 3 decide

#it seems better to keep the midterm, as unhappy with my grade that I am




plot(mylm, which = 1:2)
plot(mylm$residuals)



plot(Final.Exam~Math.425.Midterm, data = midata2)
abline(a=0,b=1)

