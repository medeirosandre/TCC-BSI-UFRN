
#######################
###     Imports     ###
#######################


# Import necessary libraries
library("FNN")          # K-NN
library("factoextra")   # feature selection funcions
library("plyr")         # rename levels
library("readr")        # read files

# Used data frames locations (dirty and clean)
dataFramesLocation <- c("tcc/databases/", "tcc/cleanDatabases/")

# Used data frames
dataFramesNames <- c("carEvaluation", "cervicalCancer", "creditApproval", "internetAds", "molecularBiology")
