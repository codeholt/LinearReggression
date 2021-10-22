library(tidyverse)
library(mosaic)
library(car)




## Simulating Data from a Regression Model
## This R-chunk is meant to be played in your R Console.
## It allows you to explore how the various elements
## of the regression model combine together to "create"
## data and then use the data to "re-create" the line.

set.seed(101) #Allows us to always get the same "random" sample
#Change to a new number to get a new sample

n <- 30 #set the sample size

X_i <- runif(n, 15, 45) 
#Gives n random values from a uniform distribution between 15 to 45.

beta0 <- 3 #Our choice for the y-intercept. 

beta1 <- 1.8 #Our choice for the slope. 


sigma <- 2.5 #Our choice for the std. deviation of the error terms.


epsilon_i <- rnorm(n, 0, sigma) 
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.

Y_i <- beta0 + beta1*X_i + epsilon_i 
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i) 
#Store the data as data

View(fabData) 


#In the real world, we begin with data (like fabData) and try to recover the model that 
# (we assume) was used to created it.

fab.lm <- lm(y ~ x, data=fabData) #Fit an estimated regression model to the fabData.

summary(fab.lm) #Summarize your model. 

plot(y ~ x, data=fabData) #Plot the data.

abline(fab.lm) #Add the estimated regression line to your plot.


# Now for something you can't do in real life... but since we created the data...

abline(beta0, beta1, lty=2) 
#Add the true regression line to your plot using a dashed line (lty=2). 

legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") 
#Add a legend to your plot specifying which line is which.

####Creating Quadradic data


## Simulating Data from a Regression Model
## This R-chunk is meant to be played in your R Console.
## It allows you to explore how the various elements
## of the regression model combine together to "create"
## data and then use the data to "re-create" the line.

set.seed(101) #Allows us to always get the same "random" sample
#Change to a new number to get a new sample

n <- 100 #set the sample size

X_i <- runif(n, -5, 8) 
#Gives n random values from a uniform distribution between 15 to 45.

beta0 <- -2 #Our choice for the y-intercept. 

beta1 <- 3 #Our choice for the slope. 

beta2 <- -0.5 #for quadradic model

sigma <- 2.5 #Our choice for the std. deviation of the error terms.


epsilon_i <- rnorm(n, 0, sigma) 
#Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.

Y_i <- beta0 + beta1*X_i + beta2*X_i^2 + epsilon_i 
#Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i) 
#Store the data as data

View(fabData) 


#In the real world, we begin with data (like fabData) and try to recover the model that 
# (we assume) was used to created it.

fab.lm <- lm(y ~ x+I(x^2), data=fabData) #Fit an estimated regression model to the fabData.

summary(fab.lm) #Summarize your model. 

b <- fab.lm$coefficients #gets all 3 coefficients

plot(y ~ x, data=fabData) #Plot the data.

#adds estimated curve to the plot
curve(b[1]+b[2]*x+b[3]*x^2,add=TRUE)

# Now for something you can't do in real life... but since we created the data...

curve(beta0+beta1*x+beta2*x^2,add=TRUE, lty=2) 
#Add the true regression line to your plot using a dashed line (lty=2). 

legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") 
#Add a legend to your plot specifying which line is which.

