library(tidyverse)

weatherData <- read.csv("C:/Users/Cody/Documents/College/Math 425/LogisticReggression/Week 1/WeatherData.csv")
targetData <- read.csv('C:/Users/Cody/Documents/College/Math 425/LogisticReggression/Week 1/target weather.csv')

#View Datasets
View(weatherData)
View(targetData)
View(aggWeathDat)
View(completeDat)

#averages the daily high temperatures for the past 3 days
aggWeathDat <- weatherData %>% 
  select(c(year,High)) %>% 
  group_by(year) %>% 
  summarise(mean = mean(High))
  
#joins the datasets
completeDat <- aggWeathDat %>% 
  left_join(targetData, by = "year") %>% 
  filter(year<2021)

weatherLM <- lm(high~mean,data=completeDat)
weatherLM


summary(weatherLM)

#graph of the reggression model
completeDat %>% 
  ggplot(aes(x=mean,y=high))+
  geom_point(alpha =1,color="skyblue", fill = "black")+
  geom_smooth(method="lm",se=F, color="black", size = .5)+
  geom_point(aes(x=77,y=predVal[1], size = 1.25), color="firebrick",show.legend=FALSE)+
  geom_text(x=77,y=60, label="Predicted Temperature for 9/20/2021 \n 77 Degrees", color="black")+
  geom_segment(x=77,y=predVal[1]-1,xend=77,yend=63)+
  geom_segment(aes(x=77,xend=77,y=47.19676,yend=95.67647), 
               size=4, color='firebrick', alpha=.05)+
  labs(title="High Temperature Prediction for 9/20", 
       x="Average High Temperature for 9/16 - 9/18", 
       y="High Temperature for 9/20")+
  theme_bw()

predVal <- predict(weatherLM, data.frame(mean = 77), interval = 'predict')
predVal
