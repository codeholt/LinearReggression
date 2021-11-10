library(tidyverse)
library(car)
library(mosaic)
rbdata1 <- read.csv("rbdata.csv")


#model
mylm1 <- lm(y~x7+
              x6+x6:x7+
              x5:I(x7^3)+
              x2+x2:x7,data=rbdata1)
summary(mylm1)
pairs(cbind(R=mylm1$residuals,fit=mylm1$fitted.values,rbdata1),pch=16, panel=panel.smooth,
      col = interaction(rbdata1$x6,rbdata1$x2))
#x7:x6 showed some significance
#uses polynomial x variable transformations
#no y transformations
#r^2 over point 8
#2 or more on off switches (at least 4 lines)
#x9 is an on off switch

rbdata1 %>% 
  ggplot(aes(x=x7,y=y,color=interaction(x6,x5,x2)))+
  geom_point()
