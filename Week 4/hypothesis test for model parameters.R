library(tidyverse)
library(car)
View(Ecdat::Caschool)
Caschool <- Ecdat::Caschool
View(Caschool)


lunch.lm <- lm(testscr~mealpct, data = Caschool)
summary(lunch.lm)

ggplot(data=Caschool,aes(x=mealpct,y=testscr))+
  geom_point()+
  geom_smooth(method='lm', se=F)
max(Caschool$testscr)
min(Caschool$testscr)


confint(lunch.lm)

par(mfrow=c(1,3))
plot(lunch.lm,which = 1:2)
plot(lunch.lm$residuals)


###
#question2


clothing <- Ecdat::Clothing
View(clothing)
?Ecdat::Clothing
cloth.lm <- lm(tsales~hourspw,data=clothing)
summary(cloth.lm)


ggplot(data=clothing, aes(x=hourspw,y=tsales))+
  geom_point()+
  geom_smooth(method='lm', se=F)


#ttest for on sample against mean
#intercep/Standard Error
?t.test
tval <- (cloth.lm$coefficients[1]-1500)/67479
pt(q=tval,df=398,lower.tail = F)
tval

pt(q=-0.0121306,df=398,lower.tail = F)
(cloth.lm$coefficients[2]-35000)/3320

confint(cloth.lm)


par(mfrow=c(1,3))
plot(cloth.lm,which = 1:2)
plot(cloth.lm$residuals)

clothing[-397]


plot(tsales~exp(hourspw), data=clothing)
cloth2.lm <- lm(log(tsales)~hourspw,data=clothing[-397,])

ggplot(data=clothing[-397,],aes(x=hourspw,y=tsales))+
  geom_point()#+
  geom_smooth(method='lm', se=F)

summary(cloth2.lm)
par(mfrow=c(1,1))
plot(cloth2.lm,which = 1:2)
plot(cloth2.lm$residuals)

boxCox(cloth2.lm)
min(clothing$hourspw)

