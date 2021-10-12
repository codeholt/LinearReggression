library(tidyverse)
library(car)


n <- 100 #sample size
Xstart <- 30 #lower-bound for x-axis
Xstop <- 100 #upper-bound for x-axis
Ystart <- 0 #min(0,min(Y)) 
Ystop <- 500 #max(max(Y), 0)
Yrange <- Ystop - Ystart

beta_0 <- 2 #choice of true y-intercept
beta_1 <- 3.5 #choice of true slope
sigma <- 13.8 #choice of st. deviation of error terms

## End of Editable area.
##-----------------------------------------------


# Create X, which will be used in the next R-chunk.
X <- rep(seq(Xstart,Xstop, length.out=n/2), each=2) 


N <- 5000 #number of times to pull a random sample
storage_b0 <- storage_b1 <- storage_rmse <- rep(NA, N)
for (i in 1:N){
  Y <- beta_0 + beta_1*X + rnorm(n, 0, sigma) #Sample Y from true model
  mylm <- lm(Y ~ X)
  storage_b0[i] <- coef(mylm)[1]
  storage_b1[i] <- coef(mylm)[2]
  storage_rmse[i] <- summary(mylm)$sigma
}



#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2,2), heights=c(3,3))
par(mfrow=c(1,1))
data = data.frame(X = X, Y = Y)
View(data)

#graph for intercept deviation
ggplot(data,aes(x=X,y=Y))+
  stat_function(fun=function(x) beta_0+beta_1*x, size = 2.5, color="green")+
  stat_function(fun=function(x) beta_0+sigma+beta_1*x, size = 2, color="limegreen", alpha = .75)+
  stat_function(fun=function(x) beta_0+sigma*2+beta_1*x, size = 1.5, color="darkgreen", alpha = .5)+
  stat_function(fun=function(x) beta_0+sigma*3+beta_1*x, size = 1, color="forestgreen", alpha = .25)+
  #for - std error
  stat_function(fun=function(x) beta_0-sigma+beta_1*x, size = 2, color="limegreen", alpha = .75)+
  stat_function(fun=function(x) beta_0-sigma*2+beta_1*x, size = 1.5, color="darkgreen", alpha = .5)+
  stat_function(fun=function(x) beta_0-sigma*3+beta_1*x, size = 1, color="forestgreen", alpha = .25)+
  # plot the points
  geom_point(color = "black", alpha = .85)+
  theme_bw()+
  geom_smooth(method='lm', alpha=1.0)
View(data)


#graph for Intercept deviation
ggplot(data, aes(x=X,y=Y))+
  geom_point()+
 +
  theme_bw()

plot(Y~X, col="black", pch=16, xlim=c(Xstart-5, Xstop+5),ylim=c(Ystart-5, Ystop+5))
for (i in 1:N){
  abline(storage_b0[i], storage_b1[i], col="darkgray")
}
abline(beta_0,beta_1, col="red")

