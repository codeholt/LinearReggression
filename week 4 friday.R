library(tidyverse)
library(car)

#calculate pval
tval <- 1.285
df <- 13

pt(-abs(tval),df)*2

pvalCalc(2.991,48)[2]

#calculating t value given confidence interval
qt(1-.05/2,48)
qt(1-.10/2,13)


cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)
confint(cars.lm)

confint(cars.lm)


#calculates tval for upper bound of intercept by hand
-17.5791+qt(1-.05/2,48)*6.7854

#calculates tval for lower bound of intercept by hand
-17.5791-qt(1-.05/2,48)*6.7854

#graph from quiz
curve(dt(x, 3), from=-4, to=4, lwd=2)
curve(dnorm(x), add=TRUE, col="gray")
abline(h=0, v=c(-1,1), col=c("gray","orange","orange"), lwd=c(1,2,2))
