library(tidyverse)
library(mosaic)
library(car)

#odds are x/(n-x) x = number of times event happens, n = total times tested
#odds give successes per failure
#odds can be over one
#probability is is x/n
#probability is successes out of total events
#probably is always between 0 and 1


#exp(stuff)/(1-exp(stuff)) = 1/(exp(-stuff)+1)

#predict statement for logistic regression
#predict(model, new data, type = "response")

#For Positive coef: as x increases by 1, the odds increase by e^beta_1
#For Negative Coef: as x increases by 1, the odds decrease by 1-e^beta_1


mylm <- lm(gasbill~month+I(month)^2,data=Utilities)
summary(mylm)
b <- coef(mylm)

plot(gasbill~month, data=Utilities,pch=21)

ggplot(data=Utilities,aes(x=month,y=gasbill))+
  geom_point(fill = "steelblue2",color = "skyblue", pch=21)+
  geom_smooth(method = 'lm', se=F, formula = y~poly(x,2))+
  stat_function(fun=function(x) b[1]+b[2]*x+b[3]*x^2)

#did not converge means clean split

#logistic regression
myglm <- glm(gasbill>80~month+I(month^2),
             data=Utilities, family = "binomial")
summary(myglm)
b2 <- coef(myglm)

col2rgb("skyblue2")/255

plot(gasbill>80~month, data=Utilities,pch=21,
     bg=rgb(0.4941176,0.7529412,0.93333330,0.1), col = "skyblue")

curve(exp(b2[1]+b2[2]*x+b2[3]*x^2)/(1+exp(b2[1]+b2[2]*x+b2[3]*x^2)),add=TRUE
      ,col = rgb(0.4941176,0.7529412,0.93333330,0.8))


ggplot(data=Utilities,aes(x=month,y=as.numeric(gasbill>80)))+
  geom_point(fill = "steelblue2",color = "skyblue", pch=21)+
  geom_smooth(method = 'glm', se=F, formula = y~poly(x,2), 
              method.args = list(family="binomial"))
