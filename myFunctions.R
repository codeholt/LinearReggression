library(tidyverse)
library(car)


#calculates the p value from a test statistic and returns one tail and two tail
#p valuess
pvalCalc <- function(tval, df){
  #one tailed result
  oneTail <- pt(-abs(tval),df)
  #two tailed result
  twoTail <- pt(-abs(tval),df)*2
  results <- c(oneTail, twoTail)
  return(results)
}

#Calculates Quantile Value (tval) from P value
quantileFromP <- function(pval, df, alpha){
  #may revisit. might be redundant
  #if (alpha == 0){
  #  result <- 
  #}
  qt(pval, df)
  #at least a note, qt(pval, degrees of freedom)
}

#calculates the Sum of Squared Errors
SSE <- function(){}

# Calculates the Sum of Squared Residuals
SSR <- function(){}

#Calculates the Total Sum of Squares
SSTO <- function(){}


