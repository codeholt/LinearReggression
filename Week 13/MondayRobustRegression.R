library(tidyverse)
library(mosaic)
library(ResourceSelection)
library(car)
library(MASS)



View(Davis)
?Davis
mylm <- lm(weight~repwt, data = Davis)
summary(mylm)

b <- coef(mylm)

#robust regression because of extreme outlier
#library MASS
robustlm <- rlm(weight~repwt, data = Davis) 
b2 <- coef(robustlm)

#midigates obvious outlier without complete removal
plot(weight~repwt, data = Davis)
ggplot(data=Davis, aes(x=repwt, y=weight))+
  geom_point(color = "goldenrod")+
  stat_function(fun = function(x) b[1]+b[2]*x, aes(color = "skyblue", group = "SLR"))+
  stat_function(fun = function(x) b2[1]+b2[2]*x, aes(color = "firebrick", group = "Robust"))+
  #Manually assignes the color a name in the legend
  scale_color_manual("Regression Lines", values = c("SLR Line" = "skyblue", "Robust" = "firebrick"))+
  theme_bw()


par(mfrow=c(2,2), mai=c(.5,.5,.5,.1))

plot(mylm, which=c(1,4))


#on top of both curves means high cooks value
#farther right means has higher potential power to change line

#watch out for high leverage and high cooks values
plot(robustlm, which=c(1,4))

plot(mylm, which=5)

plot(robustlm, which=5)

#Large cooks distance is troubling, large residuals isnt always





