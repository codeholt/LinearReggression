library(tidyverse)
library(mosaic)
library(car)
library(pander)
#overall qualityis important
#bin while coloring
#teachers data x8=0 was important in battleship

train <- read.csv("train.csv", stringsAsFactors = TRUE)

#mutate all of the columns
mytrain1 <- train %>% 
  mutate(TotalSF = X1stFlrSF + X2ndFlrSF + TotalBsmtSF,
         #turn them into characters because Tidyverse likes it better that way
         MSZoning = as.character(MSZoning),
         MSZoning2 = as.factor(case_when(
           MSZoning %in% c("RH","RL","RM","RP")~"R", 
           TRUE~MSZoning)) , 
         Alley = as.character(Alley),
         Alley = as.factor(replace_na(Alley, "NoAccess")),
         #bins the quality number into low medium and high
         OverallQual = as.character(OverallQual),
         QualRank = as.factor(case_when(
                              OverallQual %in% c('1','2','3','4')~"Low",
                              OverallQual %in% c('5','6','7')~"Med",
                              OverallQual %in% c('8','9','10')~"High",
                              TRUE~OverallQual)),
         #Bins SaleCondition to Normal,Partial, and Other
         SaleCondition = as.character(SaleCondition),
         SaleCondition2 = as.factor(case_when(SaleCondition %in% c("AdjLand", "Alloca", "Family")~"Other",
                                              TRUE~SaleCondition)),
         #changes them back
         OverallQual = as.numeric(OverallQual),
         MSZoning = as.factor(MSZoning),
         Alley = as.factor(Alley))

#splits the data into train and test sets
set.seed(121)
keep <- sample(1:nrow(mytrain1), 900)

my_train <- mytrain1[keep,]
my_test <- mytrain1[-keep,]


lm1 <- lm(SalePrice~TotalSF
            +I(TotalSF^2)
            +I(TotalSF^2):QualRank
            +SaleCondition2
          ,data=my_train)
summary(lm1)
b <- coef(lm1)

plot(SalePrice~TotalSF, data = mytrain1, col = QualRank)
curve(b['(Intercept)'] + b["TotalSF"]*x+b["I(TotalSF^2)"]*x^2, add = TRUE)
curve((b['(Intercept)']) + 
        (b["TotalSF"])*x+
        (b["I(TotalSF^2)"]+b['I(TotalSF^2):QualRankLow']) *x^2, add = TRUE, col = "red")
curve((b['(Intercept)']) + 
        (b["TotalSF"])*x+
        (b["I(TotalSF^2)"]+b['I(TotalSF^2):QualRankMed']) *x^2, add = TRUE, col = "green")



plot(lm1$residuals~ . , data= mytrain1)

####Journal
#started with total square footage in class
#starting over because I didnt really have a plan or remember what I did
#colored by quality, appears to have some influence on the price in a linear fashion
#binned into low medium and high changes the slope, but not intercept, all significant
#change is significant on x^2 term, all negative opening though, I blame "Those Two Fuckers"
#SaleCondition was significant, but only adjusted R^2 by .01. might need binning
#maybe bin to partial, Normal, and Other
#Binning didnt help at all. same results just with "Other" being insignificant

#location is important. increases price
#recently renovated
#square footage value
#repairs needed

#Looking for "Those two fuckers"
#built post 2000
#RoofStyle = "Hip"
#RoofMatl = ClyTile?, #ClyTile was a roof tile made of clay
#MasVnrType = Stone
#Fireplace Qty
#FinishGarage
#New Houses for some reason #SaleCondition (The two in question were not complete when sold)
#house was evaluated and sold before it was finished


#residuals to look into
#MSSubCLass
#MSzoning
#Neighborhood
#condition2
#houseStyle
#overallQual
#RoofMatl
#Exterior 2nd
#ExterQual
#foundation
#Has a basement vs not?
#heating
#has Second Floor?

##### Validation Code
yhat <- predict(lm1, newdata=my_test)
# Compute y-bar
ybar <- mean(my_test$SalePrice) #Yi is given by Ynew from the new sample of data

# Compute SSTO
SSTO <- sum( (my_test$SalePrice - ybar)^2 )


SSE <- sum( (my_test$SalePrice - yhat)^2 )


rsActual <- 1-SSE/SSTO

n <- length(my_test$SalePrice) #sample size

pActual <- length(coef(lm1))

rsAa <- 1 - (n-1)/(n-pActual)*SSE/SSTO

my_output_table2 <- data.frame(Model = c("Actual"), `Original R2` = c(summary(lm1)$r.squared), `Orig. Adj. R-squared` = c(summary(lm1)$adj.r.squared), `Validation R-squared` = c(rsActual), `Validation Adj. R^2` = c(rsAa))

my_output_table2 %>% pander()


