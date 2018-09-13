
#' Imports packages and sets dataframes locations and names

# install.packages("FNN")
# install.packages("factoextra")

#' Implements K-nn
library("FNN")
#' Rename levels
library("plyr")
#' Read files
library("readr")

dataframes.location <- c("tcc/originalDatasets/", "tcc/cleanDatasets/")
dataframes.names <- c("mice")
