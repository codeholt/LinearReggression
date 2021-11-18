set.seed(314) #This ensures the randomness is the "same" everytime if you play the entire R-chunk as one entire piece of code. If you run lines separately, your data might not come out the same every time. You can pick any integer value you want for set.seed. Each choice produces a different sample, so you might want to play around with a few different choices.

## To begin, decide on your sample size. (You may have to revise it later to ensure all values in your lm(...) are significant.)

n <- 50

startDomain <- -1
endDomain <- 3
## Then, create 10 X-variables using functions like rnorm(n, mean, sd), rchisq(n, df), rf(n, df1, df2), rt(n, df), rbeta(n, a, b), runif(n, a, b) or sample(c(1,0), n, replace=TRUE)... ## To see what any of these functions do, run codes like hist(rchisq(n, 3)). These functions are simply allowing you to get a random sample of x-values. But the way you choose your x-values can have quite an impact on what the final scatterplot of the data will look like.

x1 <- runif(n,startDomain,endDomain) #Dummy
x2 <- sample(c(0,1), n, replace=T) #Dummy
x3 <- sample(c(0,1), n, replace=T) #On Off switch #3
x4 <- sample(c(0,1), n, replace=T) #dummy
x5 <- sample(c(0,1), n, replace=T) #on off switch #1
#x6 <- x4+runif(n,-4,4) #NOT LEGAL. x4 actually used in model. no longer 2d drawable
x6 <- x5+runif(n,startDomain,endDomain)#Dummy
x7 <- runif(n,startDomain,endDomain) #Explanitory variable
x8 <- runif(n,startDomain,endDomain)#dummy
x9 <- sample(c(0,1), n, replace=T)#On Off switch #2
x10 <- runif(n,startDomain,endDomain) #dummy

#sample(c(0,1), n, replace=T, prob = c(.3,.7))
## Then, create betas, sigma, normal error terms and y

# *** WRITE IT DOWN BEFORE TYPING IT***
beta0 <- 1
beta1 <- .4

beta2 <- -2.2
beta3 <- 0.3

beta4 <- -1.1
beta5 <- -0.2

beta6 <- -0.1
beta7 <- -0.9

beta8 <- 3.8
beta9 <- -1.1

beta10 <- 2.1
beta11 <- 0 #not significant, no interaction between x5:x9

beta12 <- 1.3
beta13 <- 1.2

beta14 <- -3.7
beta15 <- .1
#...

sigma <- 0 #change to whatever positive number you want


################################
# You ARE NOT ALLOWED to change this part:
epsilon_i <- rnorm(n, 0, sigma)
################################ 

#An example of how to make Y...
# y <-  beta0 + beta1*X1 + beta2*X2 + beta3*X4*X2 + epsilon_i
#switches (in order) x5,x9,x3
y <- beta0+beta1*x7+
  #single interaction
  beta2*x5+beta3*x5*x7+
  beta4*x9+beta5*x9*x7+
  beta6*x3+beta7*x3*x7+
  #double Interaction
  beta8*x5*x9+beta9*x5*x9*x7+
  beta10*x5*x3+#beta11*x5*x3*x7+
  beta12*x9*x3+beta13*x9*x3*x7+
  #triple interaction
  beta14*x5*x9*x3+beta15*x5*x9*x3*x7+
  epsilon_i#...edit this code and replace it with your model. Don't forget the + epsilon_i!


## Now, you need to load your x-variables and y-variable 
## into a data set.
# You can include Y' or X' instead of Y or X if you wish.
# Remember, only these functions are allowed when transforming
# variables: 1/Y^2, 1/Y, log(Y), sqrt(Y), sqrt(sqrt(Y)), Y^2, Y^3, 1/X^2, 1/X, log(X), sqrt(X), sqrt(sqrt(X)), X^2, X^3, X^4, X^5. 
#########################################################
# ILLEGAL: Y = (beta0 + beta1*X5)^2 + epsilon_i #########
#########################################################
# Legal: sqrt(Y) = beta0 + beta1*X5^2 + epsilon_i #######
#########################################################
# You can only transform individual terms, not groups of terms.
# And the beta's cannot be part of the x-transformations.

# This loads your data into a data set:
rbdata <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
View(rbdata)
pairs(rbdata)
#Now fit your model to make sure it comes out significant:
mylm <- lm(y ~ x7+
             x5+x7:x5+
             x9+x7:x9+
             x3+x7:x3+
             x5:x9+x7:x5:x9+
             x5:x3+
             x9:x3+x7:x9:x3+
             x5:x9:x3+x7:x5:x9:x3, data=rbdata) #edit this code to be your true model
summary(mylm) %>% pander()
#all p-values must be significant
#the R^2 value must be greater than or equal to 0.30.


# Once you are done with creating your model, and have successfully
# graphed it (see below), un-comment the following `write.csv` code,
# then, PLAY this ENTIRE R-chunk to write your data to a csv.
b <- coef(mylm)
palette(c("skyblue","skyblue4","orange","orangered", "firebrick", "firebrick4", "green", "limegreen"))
plot(y~x7, data=rbdata, col = interaction(factor(rbdata$x5),factor(rbdata$x9),factor(rbdata$x3)), pch = 16)

abline(b[1],b[2], col = palette()[1])#X5=0,X9=0,X3=0
#double interaction
abline(b[1]+b[3],b[2]+b[6], col = palette()[2])#X5=1,X9=0,X3=0
abline(b[1]+b[4],b[2]+b[7], col = palette()[3])#X5=0,X9=1,X3=0
abline(b[1]+b[5],b[2]+b[8], col = palette()[5])#X5=0,X9=0,X3=1
abline(b[1]+b[3]+b[4]+b[9],b[2]+b[6]+b[7]+b[12], col = palette()[4])#X5=1,X9=1,X3=0
abline(b[1]+b[3]+b[5]+b[10],b[2]+b[6]+b[8], col = palette()[6])#X5=1,X9=0,X3=1
abline(b[1]+b[4]+b[5]+b[11],b[2]+b[7]+b[8]+b[13], col = palette()[7])#X5=0,X9=1,X3=1
abline(b[1]+b[3]+b[4]+b[5]+b[9]+b[10]+b[11]+b[14],b[2]+b[6]+b[7]+b[8]+b[12]+b[13]+b[15], col = palette()[8])#X5=1,X9=1,X3=1



beta0+beta1*x7+

  beta2*x5+
  beta4*x9+beta5*x9*x7+
  
  beta7*x3*x7+
  
  beta8*x5*x9+beta9*x5*x9*x7+
  beta10*x5*x3+beta11*x5*x3*x7+
  beta12*x9*x3+beta13*x9*x3*x7+
  
  beta14*x5*x9*x3+
  epsilon_i#...edit this code and replace it with your model. Don't forget the + epsilon_i!


plot(y~x7, data=rbdata, pch = 16 , col = interaction(factor(rbdata$x5),factor(rbdata$x9),factor(rbdata$x3)))

abline(b[1],b[2], col = palette()[1], lty=2)#X5=0,X9=0,X3=0
abline(b[1]+b[3],b[2], col = palette()[2], lty=2)#X5=1,X9=0,X3=0
abline(b[1]+b[4],b[2], col = palette()[3], lty=2)#X5=0,X9=1,X3=0
abline(b[1],b[2]+b[5], col = palette()[5], lty=2)#X5=0,X9=0,X3=1
abline(b[1]+b[3]+b[4]+b[6],b[2]+b[9], col = palette()[4], lty=2)#X5=1,X9=1,X3=0
abline(b[1]+b[3]+b[7],b[2]+b[5], col = palette()[6], lty=2)#X5=1,X9=0,X3=1
abline(b[1]+b[4]+b[8],b[2]+b[5]+b[11], col = palette()[7], lty=2)#X5=0,X9=1,X3=1
abline(b[1]+b[3]+b[4]+b[6]+b[7]+b[8]+b[12],b[2]+b[5]+b[9]+b[10]+b[11], col = palette()[8], lty=2)#X5=1,X9=1,X3=1

#estimates
abline(beta0,beta1, col = palette()[1], lty=1)#X5=0,X9=0,X3=0
abline(beta0+beta2,beta1, col = palette()[2], lty=1)#X5=1,X9=0,X3=0
abline(beta0+beta4,beta1, col = palette()[3], lty=1)#X5=0,X9=1,X3=0
abline(beta0,beta1+beta7, col = palette()[5], lty=1)#X5=0,X9=0,X3=1
abline(beta0+beta2+beta4+beta8,beta1+beta9, col = palette()[4], lty=1)#X5=1,X9=1,X3=0
abline(beta0+beta2+beta10,beta1+beta7, col = palette()[6], lty=1)#X5=1,X9=0,X3=1
abline(beta0+beta4+beta12,beta1+beta7+beta13, col = palette()[7], lty=1)#X5=0,X9=1,X3=1
abline(beta0+beta2+beta4+beta8+beta10+beta12+beta14,beta1+beta7+beta9+beta11+beta13, col = palette()[8], lty=1)#X5=1,X9=1,X3=1

