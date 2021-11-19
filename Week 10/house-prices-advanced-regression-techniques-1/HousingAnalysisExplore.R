library(tidyverse)
library(mosaic)
library(car)
#overall qualityis important
#bin while coloring
#teachers data x8=0 was important in battleship

train <- read.csv("train.csv", stringsAsFactors = TRUE)

mytrain1 <- train %>% 
  mutate(TotalSF = X1stFlrSF + X2ndFlrSF + TotalBsmtSF,
         #turn them into characters because Tidyverse likes it better that way
         MSZoning = as.character(MSZoning),
         MSZoning2 = as.factor(case_when(
           MSZoning %in% c("RH","RL","RM","RP")~"R", 
           TRUE~MSZoning)) , 
         Alley = as.character(Alley),
         Alley = as.factor(replace_na(Alley, "NoAccess")),
         #changes them back
         MSZoning = as.factor(MSZoning),
         Alley = as.factor(Alley))


lm1 <- lm(SalePrice~TotalSF+I(TotalSF^2)+OverallQual+SaleCondition+Neighborhood, data=mytrain1)
summary(lm1)

plot(SalePrice~TotalSF, data = mytrain1)

plot(lm1$residuals~ . , data= mytrain1)


#Looking for "Those two fuckers"
#built post 2000
#RoofStyle = "Hip"
#RoofMatl = ClyTile?
#MasVnrType = Stone
#Fireplace Qty
#FinishGarage
#New Houses for some reason
#SaleCondition (The two in question were not complete when sold)

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



