library(tidyverse)
library(latex2exp)

#loading in the datasets
weatherData <- read.csv("C:/Users/Cody/Documents/College/10th semester (fall 2021)/Math 425 (linear Regression)/Statistics-Notebook-master/Statistics-Notebook-master/Analyses/Linear Regression/WeatherData.csv")
targetData <- read.csv('C:/Users/Cody/Documents/College/10th semester (fall 2021)/Math 425 (linear Regression)/Statistics-Notebook-master/Statistics-Notebook-master/Analyses/Linear Regression/target weather.csv')

aggWeathDat <- weatherData %>% 
  select(c(year,High)) %>% 
  group_by(year) %>% 
  summarise(mean = mean(High))

#joins the datasets
completeDat <- aggWeathDat %>% 
  left_join(targetData, by = "year") %>% 
  filter(year<2021) %>% 
  filter(high>60)

View(completeDat)
#preps the regression for prediction
weatherLM <- lm(high~mean,data=completeDat)
#base plot
p<- completeDat %>% 
  ggplot(aes(x=mean,y=high))+
  #plots regression line
  geom_smooth(method='lm', se=FALSE, color = "black")+
  labs(title="High Temperature Prediction for 9/20", x="Average High Temperature for 9/16 - 9/18", y="High Temperature for 9/20")+
  theme_bw()+
  #plots data
  geom_point()


#### Section one code
#What is a residual? What use does a single residual provide within a regression analysis?
#Be sure to demonstrate these ideas with graphics, mathematical equations, and written explanations.
#For example, if you compare your laptop to some subset of the laptops data set, did you pay over, under, 
#or right on average for what similar laptops cost? 



predVal <- predict(weatherLM, data.frame(mean=74))

#adds annotations to important things
p+
  #plots and annotates residual line
  geom_segment(x=74,y=72.75,xend=74,yend=81, color="red", size = 1.25)+
  annotate('text', x=70,y=77,label="Residual~error~(epsilon[i])~8.2", parse=TRUE, size=5)+
  #Higlights predicted value
  geom_point(aes(x=74,y=predVal), size = 3)+
  annotate('text',x=78,y=72,label=TeX("$72.8\\,Predicted\\,Value(\\hat{Y}_i)$"), size=5)+
  
  #highlights the point I want
  geom_point(aes(x=74,y=81), size = 2.5, color='darkgreen')+
  annotate('text', x=73,y=81,label="Y[i]~81",parse=TRUE,size=5)

#######
#Question 2
#######
#2. What are each of SSTO, SSR, and SSE? How are they related? How do they differ? Find a way to both show and explain these values. 
#How are they used to gain insight about data within a regression analysis?
#Be sure to present a graphic demonstrating each of these concepts as well as their respective mathematical formulas. 
#Written explanations are also powerful in revealing these concepts.

#SSE

p+
  #adds the residuals for all points
  geom_rect(aes(xmin=mean,ymin=weatherLM$fitted.values,xmax=mean+weatherLM$residuals,ymax=weatherLM$fitted.values+weatherLM$residuals)
               ,fill="steelblue", size = 1.25, alpha=.5)


#SSR
p+
  #adds SSR
  geom_segment(x=completeDat$mean, xend=completeDat$mean, y=weatherLM$fitted.values, yend=mean(completeDat$high), color='firebrick')+
  annotate('text',x=83,y=76,label=TeX("$Prediction\\,(\\hat{Y})$"), size=5)+
  annotate('text',x=65,y=74,label=TeX("$Average\\,Y\\,Value\\,(\\bar{Y})$"), size=5)+
  geom_hline(yintercept = mean(completeDat$high), linetype='dashed')

#SSTO
p+
  geom_rect(aes(xmin=mean,ymin=weatherLM$fitted.values,xmax=mean+weatherLM$residuals,ymax=weatherLM$fitted.values+weatherLM$residuals),fill="steelblue", size = 1.25,alpha=.75)+
  geom_rect(xmin=completeDat$mean, xmax=completeDat$mean+weatherLM$residuals, ymin=weatherLM$fitted.values, ymax=mean(completeDat$high),fill='firebrick', alpha=.5)+
  annotate('text',x=83,y=76,label=TeX("$Prediction\\,(\\hat{Y})$"), size=5)+
  annotate('text',x=65,y=74,label=TeX("$Average\\,Y\\,Value\\,(\\bar{Y})$"), size=5)+
  geom_hline(yintercept = mean(completeDat$high), linetype='dashed')



