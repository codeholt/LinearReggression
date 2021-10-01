library(tidyverse)



log(5000)
exp(8.5171932)
exp(1)


head(islands)
?islands
df <- data.frame(as.list(islands))
df

View(islands)

islands %>% 
  ggplot(aes(x=value))+
  geom_histogram()


hist(islands)
hist(log(islands))

exp(4)
log(10000)


library(mosaicData)
View(Utilities)
?Utilities

#no transformation
ggplot(data=Utilities,aes(x=temp,y=gasbill))+
  geom_point()+
  geom_smooth(method='lm')
gasbill.lm <- lm(gasbill~temp,data=Utilities)
summary(gasbill.lm)

par(mfrow=c(1,3))
plot(gasbill.lm,which = 1:2)
plot(gasbill.lm$residuals)


#y transformed
ggplot(data=Utilities,aes(x=temp,y=log(gasbill)))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

gasbilllm.log <- lm(log(gasbill)~temp,data=Utilities)

par(mfrow=c(1,3))
plot(gasbilllm.log,which = 1:2)
plot(gasbilllm.log$residuals)


b <- gasbilllm.log$coefficients
#not transformed with transformed line
ggplot(data=Utilities,aes(x=temp,y=gasbill))+
  geom_point()+
  stat_function(fun=function(x) exp(b[1]+b[2]*x))+
  geom_smooth(method='lm', se=F)
  

summary(gasbilllm.log)
boxCox(gasbill.lm)

par(mfrow=c(1,1)
)
    