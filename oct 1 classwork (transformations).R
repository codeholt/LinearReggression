library(tidyverse)
library(mosaic)

YoungOrange <- Orange %>% 
  filter(age<1200)

orange.lm <- lm(circumference~age, data=YoungOrange)
orangeLog.lm <- lm(log(circumference)~age, data=YoungOrange)
orangeSqrt.lm <- lm(sqrt(circumference)~age, data=YoungOrange)
orangeOver.lm <- lm((1/circumference)~age, data=YoungOrange)
orange2.lm <- lm(circumference^2~age, data=YoungOrange)
orangeNeg2.lm <- lm(circumference^-2~age, data=YoungOrange)

b <- orange.lm$coefficients
bLog <- orangeLog.lm$coefficients
bSqrt <- orangeSqrt.lm$coefficients
bOver <- orangeOver.lm$coefficients
b2 <- orange2.lm$coefficients
bNeg2 <- orangeNeg2.lm$coefficients

#base plot
plot <- ggplot(YoungOrange, aes(x=age, y=circumference, color=ifelse(age<1200, "young","old"))) + 
  geom_point(color="orangered") +
  stat_function(fun=function(x) (b[1]+b[2]*x), color = "skyblue")+
  stat_function(fun=function(x) exp(bLog[1]+bLog[2]*x), color="red")+
  stat_function(fun=function(x) 1/(bOver[1]+bOver[2]*x),color = "goldenrod")+
  stat_function(fun=function(x) sqrt(b2[1]+b2[2]*x),color="deeppink")+
  stat_function(fun=function(x) (bSqrt[1]+bSqrt[2]*x)^2,color = "coral")+
  stat_function(fun=function(x) (bNeg2[1]+bNeg2[2]*x)^-0.5,color = "forestgreen")+
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw()



#Y'= log(Y)
plot + stat_function(fun=function(x) exp(b[1]+b[2]*x))

#Y' = sqrt(Y)
plot + stat_function(fun=function(x) (b[1]+b[2]*x)^2)

#Y' = 1/y
plot + stat_function(fun=function(x) 1/(b[1]+b[2]*x))

#Y' = Y
plot + stat_function(fun=function(x) b[1]+b[2]*x)

#Y' = Y^2
plot + stat_function(fun=function(x) sqrt(b[1]+b[2]*x))

