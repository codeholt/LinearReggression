library(tidyverse)
library(mosaic)
library(car)

 View(rbdata)




#lm
mylm <- lm(y~x10+ 
             #noticed x4
             x4:x10+I(x10^2):x4+I(x10^3):x4+
             #x5
             x5+x5:x10+I(x10^2):x5+I(x10^3):x5+
             x9+x9:x10+I(x10^2):x9,data=rbdata)
summary(mylm)
b <- mylm$coefficients
pairs(cbind(R=mylm$res, fit=mylm$fit, rbdata), panel=panel.smooth,pch=16, cex=1,
      col=interaction(rbdata$x4,rbdata$x5,rbdata$x9))

plot(mylm$residuals~x10, data= rbdata)
#x10:x2 may interact if other terms are included, insignificant right now
#theres probably a cubic, quadradic, and two linear models in the data at a minimum,
#if I can get some of those out, I should be able to get close

#x4 isolates the cubic model
#x5 isoaltes the positive models
#The negative Linear model hasnt shown its face yet

#putting x4 in all steps kills significance but reduces the residuals to really low
#Ill take out the interaction but Ill save it for later

#interaction wasnt significant after all
#x5 on the other hand interacted with x10 and pulled my model up to .9898 R^2
#x5 and x4 cannot interact, niether x5:x9, nor x4:x9

#no boxCox transformation either



plot(y~x10, data=rbdata, col=interaction(x4,x5,x9))
points(mylm$fitted.values~x10, data=rbdata, col=interaction(x4,x5,x9), pch=16, size = .5, add = TRUE)
point
#my plot
palette(c("skyblue","skyblue4","orange","orangered", "firebrick", "firebrick4", "green", "limegreen"))

plot(y~x10, data=rbdata, col=interaction(x4,x5,x9), pch=16)
legend("topleft",
       legend = c(rbdata$x4,rbdata$x5,rbdata$x9),
       col = palette())
curve(b[1]+b[2]*x,col = palette()[1], add = TRUE)
#cubic being x4 added
x4 = 1
x5 = 0
x9 = 0
curve((b[1]+b[4])+(b[2]+b[5])*x*x4+(b[6])*x^2*x4+(b[7])*x^3*x4,col = palette()[2], add = TRUE)

x4 = 0
x5 = 1
x9 = 0
curve((b[1]+b[3])+(b[2]+b[8])*x*x5,col = palette()[3], add = TRUE)

x4 = 0
x5 = 0
x9 = 1
curve((b[1]+b[4])+(b[2]+b[9])*x*x9,col = palette()[4], add = TRUE)



rbdata %>% 
  ggplot(aes(shape = interaction(x4,x5,x9),color = interaction(x4,x5,x9))) + 
  scale_fill_manual(values = palette())+
  scale_color_manual(values = palette())+
  geom_point(aes(x=x10,y=y), size = 2)+
  #geom_point(aes(x=x10,y=mylm$fitted.values))+
  #x4=0.x5=0.x9=0
  stat_function(fun=function(x) b[1]+b[2]*x, geom="line", color = palette()[1])+
  #x4=1.x5=0.x9=0
  stat_function(fun=function(x) (b[1])+(b[2]+b[5])*x+(b[6])*x^2+(b[7])*x^3,
                geom="line", color = palette()[2])+
  #x4=0.x5=1.x9=0
  stat_function(fun=function(x) (b[1]+b[3])+(b[2]+b[8])*x+ b[9]*x^2+b[10]*x^3,
                geom="line", color = palette()[3])+
  #x4=0.x5=1.x9=1
  stat_function(fun=function(x) (b[1]+b[3]+b[4])+(b[2]+b[8]+b[11])*x + 
                  (b[9]+b[12])*x^2+b[10]*x^3,
                geom="line", color = palette()[4])+
  #x4=0.x5=1.x9=2
  stat_function(fun=function(x) (b[1]+b[3]+2*b[4])+(b[2]+b[8]+2*b[11])*x+ 
                  (b[9]+2*b[12])*x^2+b[10]*x^3,
                geom="line", color = palette()[5])+
  #x4=0.x5=1.x9=3
  stat_function(fun=function(x) (b[1]+b[3]+3*b[4])+(b[2]+b[8]+3*b[11])*x + 
                  (b[9]+3*b[12])*x^2+b[10]*x^3,
                geom="line", color = palette()[6])+
  theme_bw()



