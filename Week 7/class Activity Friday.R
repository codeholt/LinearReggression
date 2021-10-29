library(tidyverse)
library(mosaic)
library(car)



#controls the color base R
palette()
palette(c("skyblue", "firebrick", "orange"))
plot(hp~qsec, data=mtcars, col=as.factor(cyl))
table(mtcars$cyl)

#controls the color ggplot
ggplot(data=mtcars,aes(x=qsec,y=hp, color=as.factor(cyl)))+
  geom_point()+
  scale_color_manual(values = c("skyblue", "firebrick", "orange"),
                     name="cyl")+
  theme_bw()

#what is '*' doing?
#'*' tells both inteceot and slope. base term and intercept. x + x2 + x:x2 + x3 + x:x3...
lm1 <- lm(hp~qsec*as.factor(cyl), data=mtcars)
summary(lm1)


#ggplot knows the 3 lines using geom_smooth
ggplot(data=mtcars,aes(x=qsec,y=hp, color=as.factor(cyl)))+
  geom_point()+
  scale_color_manual(values = c("skyblue", "firebrick", "orange"),
                     name="cyl")+
  geom_smooth(method="lm",se=F,formula = y~x)+
  theme_bw()

#powerful but tedious. allows for reduced model instead of full
plot(hp~qsec, data=mtcars, col=as.factor(cyl))
b <- coef(lm1)
cyl6=0
cyl8=0
curve(b[1]+b[2]*qsec+b[3]*cyl6+b[4]*cyl8 + b[5]*cyl6*qsec+b[6]*cyl8*qsec,
      add=TRUE, xname = "qsec", col = palette()[1])
cyl6=1
cyl8=0
curve(b[1]+b[2]*qsec+b[3]*cyl6+b[4]*cyl8 + b[5]*cyl6*qsec+b[6]*cyl8*qsec,
      add=TRUE, xname = "qsec", col = palette()[2])
cyl6=0
cyl8=1
curve(b[1]+b[2]*qsec+b[3]*cyl6+b[4]*cyl8 + b[5]*cyl6*qsec+b[6]*cyl8*qsec,
      add=TRUE, xname = "qsec", col = palette()[3])

#make sure there are interactions and no NA's in the output
#also way too many lines
#If I cant visualize it, dont trust right now
View(starwars)
star.lm <- lm(mass ~ height * species, data=starwars)
summary(star.lm)

ggplot(starwars,aes(x=height,y=mass,color = species))+
  geom_point()+
  geom_smooth(method="lm",se=F,formula = y~x)

table(starwars$species)





