library(tidyverse)
library(car)
library(mosaic)


set.seed(101) #Allows us to always get the same "random" sample.
n <- 30 #set the sample size



#Gives n random values from a uniform distribution between -2 to 4.
X_i <- runif(n, -2, 4) 

beta0 <- 2 #Our choice for the y-intercept. 
beta1 <- -3 #Our choice for the slope. 
beta2 <- -1 #for quadradic model
beta3 <- .4 #for the cubic 
sigma <- 1 #Our choice for the std. deviation of the error terms.
epsilon_i <- rnorm(n, 0, sigma) 
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.
Y_i <- beta0 + beta1*X_i + beta2*X_i^2 +beta3*X_i^3 + epsilon_i 
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i) 
#Store the data as data

#In the real world, we begin with data (like fabData) and try to recover the model that 
# (we assume) was used to created it.

fab.lm <- lm(y ~ x+I(x^2)+I(x^3), data=fabData) #Fit an estimated regression model to the fabData.
summary(fab.lm) #Summarize your model. 
b <- fab.lm$coefficients #gets all 3 coefficients

plot(y ~ x, data=fabData) #Plot the data.
#adds estimated curve to the plot
curve(b[1]+b[2]*x+b[3]*x^2+b[4]*x^3,add=TRUE)
curve(beta0+beta1*x+beta2*x^2+beta3*x^3,add=TRUE, lty=2) 
#Add the true regression line to your plot using a dashed line (lty=2). 
legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") 
#Add a legend to your plot specifying which line is which.


### Two lines model

#Gives n random values from a uniform distribution between -2 to 4.
X_i1 <- runif(n, 30, 50) 
X_i2 <- sample(c(0,1),n, replace=TRUE)
beta0 <- 1 #Our choice for the y-intercept. 
beta1 <- 1.33 #Our choice for the slope. 
beta2 <- 60 #intercept interaction
beta3 <- -1.7 #slope interaction 
sigma <- 2 #Our choice for the std. deviation of the error terms.
epsilon_i <- rnorm(n, 0, sigma) 

#X_i2 <- 0
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.
Y_i1 <- beta0 + beta1*X_i1 + beta2*X_i2 +beta3*X_i1*X_i2 + epsilon_i 
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i1, x=X_i1, x2=X_i2) 
#Store the data as data
View(fabData)
#In the real world, we begin with data (like fabData) and try to recover the model that 
# (we assume) was used to created it.

fab.lm <- lm(y ~ x+x2+x:x2, data=fabData) #Fit an estimated regression model to the fabData.
summary(fab.lm) #Summarize your model. 
b <- fab.lm$coefficients #gets all 3 coefficients

plot(y ~ x,col=as.factor(x2), data=fabData) #Plot the data.
#adds estimated curve to the plot
x2 <- 0
curve(b[1]+b[2]*x+b[3]*x2+b[4]*x*x2,add=TRUE)
x2 <- 1
curve(b[1]+b[2]*x+b[3]*x2+b[4]*x*x2,add=TRUE, col='firebrick')

#Add the true regression line to your plot using a dashed line (lty=2). 
legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") 
#Add a legend to your plot specifying which line is which.
