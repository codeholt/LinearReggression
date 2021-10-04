library(tidyverse)

N <- 20
by <- 10
storage <- rep(NA,N)




for (i in 1:N){
  storage[i] <- by*i
  cat("i =", i, " and ", by, "*i =", by*i, " was saved in storage[", i, "]\n")
}


#Generate data

b0Law <- 2.5
b1Law <- 3

n <- 40
#spaces x values out evenly
#sequence( start at 30, end at 100, 
#get n(in this case 40) values spread out evenly across the range, gets 2 per x)
Xi <- rep(seq(30, 100, length.out=n/2), each=2) #n must be even.
#calculates y value
Yi <- b0Law + b1Law*Xi + rnorm(n, 0, 1.2)




mylm <- lm(Yi ~ Xi)
coef(mylm)
coef(mylm)[1] #intercept only
coef(mylm)[2] #slope only

plot(Yi~Xi)
abline(mylm)
##########################
N <- 20

slopes <- rep(NA,N)
intercepts <- rep(NA,N)

for (i in 1:N){
  v <- 40
  #spaces x values out evenly
  #sequence( start at 30, end at 100, 
  #get n(in this case 40) values spread out evenly across the range, gets 2 per x)
  Xi <- rep(seq(30, 100, length.out=v/2), each=2) #n must be even.
  #calculates y value
  Yi <- b0Law + b1Law*Xi + rnorm(v, 0, 1.2)
  mylm <- lm(Yi ~ Xi)
  slopes[i] <- coef(mylm)[2] #slope only
  intercepts[i] <- coef(mylm)[1] #intercept only
  
}
intercepts
slopes
hist(slopes)
hist(intercepts)


plot(intercepts~slopes)





