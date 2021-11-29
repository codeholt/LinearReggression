library(pander)
library(tidyverse)
library(mosaic)
library(car)


rbdata <- read.csv("saunders_Fall21_RBdata.csv", header=TRUE)
pairs(rbdata)


