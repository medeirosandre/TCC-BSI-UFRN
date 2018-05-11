
#######################
###     Imports     ###
#######################

# install.packages("FNN")
# install.packages("factoextra")

# Import necessary libraries
library("FNN")          # K-NN
library("factoextra")   # feature selection funcions
library("plyr")         # rename levels
library("readr")        # read files

# Used data frames locations (dirty and clean)
dataframes.location <- c("tcc/databases/", "tcc/cleanDatabases/")

# Used data frames
dataframes.names <- c("carEvaluation", "creditApproval", "mushroom", "internetAds")
