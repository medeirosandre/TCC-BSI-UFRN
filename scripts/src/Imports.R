
#' Imports packages and sets dataframes locations and names

# install.packages("FNN")
# install.packages("factoextra")

#' Implements K-nn
library("FNN")
#' Rename levels
library("plyr")
#' Read files
library("readr")
library("farff")

dataframes.location <- c("tcc/originalDatasets/", "tcc/cleanDatasets/")
dataframes.names <- c("1year")
