library(mosaic)
sum(1:6)
sum(1:6)^2
sum((1:6)^2)
x = c(5, 15, 2, 29, 35, 24, 25, 39)
x = c(5, 15, 2, 29, 35, 24, 25, 39)
sum(x^2)
sum(x)^2


cars.lm=lm(dist~speed,data=cars)

cars %>% 
  ggplot(aes(x=speed,y=dist))+
  geom_point(color='firebrick')+
  geom_smooth(method='lm', se=FALSE, color='black')+
  geom_hline(yintercept = mean(cars$dist), lty=2)

mean(cars$dist)
summary(cars.lm)

sum(cars.lm$residuals)^2
#residuals are estimates of error

cars.lm$fit

.65^(1/2)
sum