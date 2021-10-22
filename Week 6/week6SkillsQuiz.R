library(tidyverse)
library(car)
library(mosaic)
library(modelr)


?Utilities

#make the model
gas.lm <- lm(gasbill~month+I(month^2), data=Utilities)
summary(gas.lm)
#coefficients
gas.b <- gas.lm$coefficients

#plot with coefficients
ggplot(Utilities,aes(x=month, y=gasbill))+
  geom_point()+
  stat_function(fun = function(x) gas.b[1]+gas.b[2]*x+gas.b[3]*x^2)+
  theme_bw()

#plot the diagnostic plots
plot(gas.lm, which = 1:2)
plot(gas.lm$residuals)
qqPlot(gas.lm)

View(Utilities)
# get the prediction interval for september
predict(gas.lm,data.frame(month=9), interval = "predict")
?predict

#get the actual mean for all months
Utilities %>% 
  group_by(month) %>% 
  summarise(mean =mean(gasbill))

#difference between actual mean and regression mean
22.8-38.84658
#calculate the MSE
30.48^2
#its really not as good as by hand
mse(gas.lm, data=Utilities)


##### QUestion 2

cars.lm <- lm(mpg~qsec+am+qsec:am, data=mtcars)
cars.b <- cars.lm$coefficients
summary(cars.lm)

# plots the data
ggplot(mtcars,aes(x=qsec,y=mpg, color=as.factor(am)))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
#go back and learn how to do multiple linear regression in ggplot
  stat_function(fun=function(x) cars.b[1]+cars.b[2]*x)+
  stat_function(fun=function(x) (cars.b[1]+cars.b[3])+(cars.b[2]+cars.b[4])*x)

#base R output has exact coefficients

#didnt give right p value, diagnose later
#pt(-abs(-(1.3214/0.7017)), 28)

plot(cars.lm,which=1:2)
plot(cars.lm$residuals)

##### Question 3

#rediculous LM predicting the qsec
q.lm <- lm(qsec~disp+I(disp^2)+am+disp:am+I(disp^2):am, data=mtcars)
summary(q.lm)
q.b <- q.lm$coefficients

ggplot(mtcars,aes(x=disp,y=qsec,color=as.factor(am)))+
  geom_point()+
  stat_function(fun=function(x) q.b[1]+q.b[2]*x+q.b[3]*x^2)+
  stat_function(fun=function(x) (q.b[1]+q.b[4])+(q.b[2]+q.b[5])*x+(q.b[3]+q.b[6])*x^2)

plot(q.lm,which=1:2)
plot(q.lm$residuals)

#perform the regression and drop the disp:am term (the least significant)
q2.lm <- lm(qsec~disp+I(disp^2)+am+I(disp^2):am, data=mtcars)
summary(q2.lm)
q2.b <- q2.lm$coefficients


##### Question 4 Simulating data

#set seed and sample size
set.seed(101) #Allows us to always get the same "random" sample.
n <- 100 #set the sample size

#Gives n random values from a uniform distribution between -2 to 4.
X_i1 <- runif(n, -2, 3) 
X_i2 <- sample(c(0,1),n, replace=TRUE)

#get law coefficients

beta_0 <- -2 #base intercept
beta_1 <- 3 #base x
beta_2 <- 4#base quadradic
beta_3 <- 2-beta_0#interaction of intercept
beta_4 <- 5-beta_1 #interaction of x
beta_5 <- -3 -beta_2#quadradic interaction

sigma <- 2 #Our choice for the std. deviation of the error terms.
epsilon_i <- rnorm(n, 0, sigma)
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.
Y_i <- beta_0 + beta_1*X_i1 + beta_2*X_i1^2 +beta_3*X_i2 + beta_4*X_i1*X_i2 + beta_5*X_i1^2*X_i2 + epsilon_i 
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i1, x2=X_i2) 
#Store the data as data
View(fabData)

fab.lm <- lm(y ~ x+I(x^2)+x2+x:x2+I(x^2):x2, data=fabData) #Fit an estimated regression model to the fabData.
summary(fab.lm) #Summarize your model. 
b <- fab.lm$coefficients #gets all 3 coefficients
b
plot(y ~ x,col=as.factor(x2), data=fabData) #Plot the data.
#adds estimated curve to the plot
x2 <- 0
curve(b[1]+b[2]*x+b[3]*x^2+b[4]*x2+b[5]*x*x2+b[6]*x^2*x2,add=TRUE)
#True line
curve(beta_0+beta_1*x+beta_2*x^2+beta_3*x2+beta_4*x*x2+beta_5*x^2*x2,add=TRUE, lty=2)
x2 <- 1
curve(b[1]+b[2]*x+b[3]*x^2+b[4]*x2+b[5]*x*x2+b[6]*x^2*x2,add=TRUE, col='firebrick')
curve(beta_0+beta_1*x+beta_2*x^2+beta_3*x2+beta_4*x*x2+beta_5*x^2*x2,add=TRUE, lty=2, col='firebrick')



#Add the true regression line to your plot using a dashed line (lty=2). 
legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") 
#Add a legend to your plot specifying which line is which.






