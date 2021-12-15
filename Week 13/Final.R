library(tidyverse)
library(car)
library(mosaic)
library(MASS)
library(ResourceSelection)

#Question 1
?cats
View(cats)
cats.lm <- lm(Hwt~Bwt, data = cats)
summary(cats.lm)$sigma
plot(cats.lm)

#Question 2
stuff <- (3.86370+(-5.88493))+(-0.02532+0.05708)*50
exp(stuff)/(1+exp(stuff))

#Question 3
ccf.lm <- lm(ccf ~ thermsPerDay, data=Utilities)
cb <- coef(ccf.lm)
plot(ccf ~ thermsPerDay, data=Utilities)
abline(cb[1],cb[2])
plot(ccf.lm,which = 1:2)
plot(ccf.lm$residuals)


#question 4 Incorrect


# Use the Utilities data set to perform a regression that shows how kilowatt hours are impacted by the year and average temperature as well as their interaction.
# 
# Within this regression there are a few minor outliers, but nothing too serious as shown by the appropriate diagnostic plots. Point 14 is currently having the most influence of any of the points on the regression, though still minimal in its influence.
# 
# Which point has the second most amount of influence on the regression?

?Utilities
kwh.lm <- lm(kwh~year*temp,data=Utilities)
summary(kwh.lm)
plot(kwh.lm)

# To perform the regression:
#   
#   > lm.kwh <- lm(kwh ~ year + temp + year:temp, data=Utilities)
# > summary(lm.kwh)
# 
# To diagnose the regression we start here:
#   
#   > plot(lm.kwh, which=1)
# 
# This identifies points 14, 94, and 53 as possible outliers. However, to determine the influence a point has on the regression, we need to consider the Cook's Distance (or some other measurement like leverage). This plot gives us Cook's Distances:
#   
#   > plot(lm.kwh, which=4)
# 
# In that plot, we see that Point #1 has the second largest Cook's Distance, and thus the second largest influence on the line.


#Question 5

temp.lm <- lm(sqrt(temp)~ccf, data = Utilities)
summary(temp.lm)
tb <- coef(temp.lm)
#boxCox(temp.lm)

plot(temp~ccf, data = Utilities)
curve((tb[1] + tb[2]*x)^2, add=TRUE, col="blue")

curve((8.523 - 0.021*x)^2, add=TRUE, col="skyblue") 

#Question 6
plot(totalbill~month, data = Utilities)

util.lo <- loess(totalbill~month, data = Utilities, 
                 degree = 2, span = 0.3)
predict(util.lo, data.frame(month = 9))

summary(util.lo)


#question 8
plot(totalbill ~ gasbill, data=Utilities)
gas.lm <- lm(totalbill ~ gasbill, data=Utilities)
abline(gas.lm$coefficients[1], gas.lm$coefficients[2],add = T)

predict(gas.lm, data.frame(gasbill = 150), interval = "predict")

#Question 9
plot(gasbill~ccf, data = Utilities)
gasprice.lm <- lm(gasbill~ccf, data = Utilities)
summary(gasprice.lm)

#question 10
pairs(airquality, panel = panel.smooth)

lm(Ozone ~ Wind , data = airquality) %>% summary() #.3619
lm(Wind~Temp , data = airquality) %>% summary() #.2098
lm(Solar.R ~ Wind  , data = airquality) %>% summary() #.its negative
lm(Ozone ~ Temp  , data = airquality) %>% summary() #.48

#Question 11

pairs(Utilities, panel = panel.smooth)
pairs(cbind(R=gas.lm$res, fit=gas.lm$fit,Utilities), panel=panel.smooth, cex=1,)

#Question 12
elecbill.lm <- lm(elecbill~kwh, data = Utilities)
summary(elecbill.lm)
#estimate - hypothesis / std error
tval <- (.108754-.11)/0.005816
pt(-abs(tval), 115)*2


#Question 13
# Use the regression summary output below to obtain the 95% confidence interval for the true regression slope,
# 
# .
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 731.4507    28.1636  25.972   <2e-16 ***
#   ccf           0.2339     0.2524   0.927    0.356    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 202.2 on 115 degrees of freedom
# Multiple R-squared:  0.007409,	Adjusted R-squared:  -0.001222 
# F-statistic: 0.8585 on 1 and 115 DF,  p-value: 0.3561
# 
# (-0.266, 0.734) > Correct answer
# (-0.599, 1.067)
# (-0.094. 0.562)
# (-0.271, 0.739) > my guess, incorrect
# 
# 0.2339 - qt(1-0.05/2, 115)*0.2524 #Lower bound
# 
# 0.2339 + qt(1-0.05/2, 115)*0.2524 #Upper bound
# 
# The 115 is the degrees of freedom. The qt(...) function gives the value of t* for 95% confidence by using the 1-0.05/2 = 0.975, or 97.5%-tile. The 0.2339 is the estimate of the slope. The 0.2524 is the standard error of the slope.
# 
# To see that this matches what R would have given if you had had the lm that produced this summary output, run these codes:
#   
#   > u.lm <- lm(kwh ~ ccf, data=Utilities)
# 
# > summary(u.lm)
# 
# > confint(u.lm)
.5048+.2339
.2339-.5048


#question 14
total.lm <- lm(totalbill~kwh+ccf+year, data = Utilities)
summary(total.lm)

#Question 15
gasTherms.lm <- lm(gasbill~thermsPerDay, data = Utilities)
summary(gasTherms.lm)

gasTherms.rlm <- rlm(gasbill~thermsPerDay, data = Utilities)
summary(gasTherms.rlm)



#question 16

tempMonth.lm <- lm(temp~month+I(month^2), data = Utilities)
summary(tempMonth.lm)
tmb <- coef(tempMonth.lm)
plot(temp~month, data = Utilities)
curve(tmb[1]+tmb[2]*x+tmb[3]*x^2, add = TRUE, col = "black")


#Question 17
tempMonth3.lm <- lm(temp~month+I(month^2)+I(month^3), data = Utilities)
summary(tempMonth3.lm)
tm3b <- coef(tempMonth3.lm)
plot(temp~month, data = Utilities)
curve(tm3b[1]+tm3b[2]*x+tm3b[3]*x^2+tm3b[4]*x^3, add = TRUE, col = "black")


#Question 18 Incorrect

Prestige2 <- mutate(Prestige, type = as.factor(ifelse(type == "prof", "prof","other")))
prest.lm <- lm(income~prestige+type, data = Prestige2)
summary(prest.lm)
pb <- prest.lm$coefficients
plot(income~prestige, data = Prestige2, col = type)



# Open the Prestige data in R from library(car) and run the following code:
#   
#   > Prestige2 <- mutate(Prestige, type = as.factor(ifelse(type == "prof", "prof","other")))
# 
# Consider the regression model that is shown in the graph below for this data.
# 
# Run an appropriate analysis in R that tests to see if the slope of the two lines shown in the graph below differ significantly.
# 
# Report the p-value of the test. 
# 
# otherProfQ22.png
# 0.00116
# 0.357
# 0.00497
# 0.844
# 
# The most important thing to notice in the plot is that the y-intercepts of the two lines are equal. 
# 
# That requires that we choose the following for the lm... and the p-value is then the change in slope term type:prestige, which is 0.357 (not significant in this case).
# 
# > lm.prestige2 <- lm(income ~ prestige + type:prestige, data=Pre)
# 
# > summary(lm.prestige2)
# 
# 
# 
# Call:
#   lm(formula = income ~ prestige + type:prestige, data = Pre)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6863  -1396    168   1050  14936 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -369.26    1347.20  -0.274    0.785    
# prestige            146.70      34.50   4.252 4.95e-05 ***
#   prestige:typeprof    17.01      18.39   0.925    0.357    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3024 on 95 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.4992,	Adjusted R-squared:  0.4886 
# F-statistic: 47.34 on 2 and 95 DF,  p-value: 5.444e-15
# 


#question 20 Incorrect
?Chile
income.lm <- lm(income~population, data = Chile)
summary(income.lm)
plot(income~population, data = Chile)
plot(income.lm, which = 1:2)

plot(income.lm$residuals)

#QUestion 21
cars2 <- cars
cars2[16,]$dist <- 12
stoping.lm <- lm(dist ~ speed, data=cars)
summary(stoping.lm)

stoping2.lm <- lm(dist ~ speed, data=cars2)
summary(stoping2.lm)


#Question 24
(85.7380+(-29.8786))+(-0.1187+(.2950))*(60*2.5)


#Question 25
mtcars.lm <- lm(log(mpg)~wt, data = mtcars)
b <- coef(mtcars.lm)
predict(mtcars.lm, data.frame(wt=2.050), interval = "predict") %>% exp()
plot(mpg~wt, data = mtcars, pch =16)
curve(exp(b[1]+b[2]*x), add = TRUE)
