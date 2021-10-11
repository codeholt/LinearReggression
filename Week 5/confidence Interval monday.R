library(tidyverse)
library(car)
library(modelr)

View(faithful)

faithful.lm <- lm(waiting~eruptions,data=faithful)
summary(faithful.lm)
confint(faithful.lm)
#prediction interval
predict(faithful.lm,data.frame(eruptions=2), interval = "prediction")
#confidence interval
predict(faithful.lm,data.frame(eruptions=2), interval = "confidence")

ggplot(data=faithful,aes(x=eruptions,y=waiting))+
  geom_point(color="steelblue")+
  geom_smooth(method="lm",se=F, color = "darkgray")+
  theme_bw()
#grey area lives in gray reigon when se=T
#if a flat line can exist in the reigon, the slope isnt significant

?lm()
mse(faithful.lm, faithful)
rmse(faithful.lm, faithful)
