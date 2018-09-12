
#######################
###     Imports     ###
#######################

# install.packages("FNN")
# install.packages("factoextra")

# Import necessary libraries
library("FNN")          # K-NN
library("plyr")         # rename levels
library("readr")        # read files

# Used data frames locations (dirty and clean)
dataframes.location <- c("tcc/originalDatasets/", "tcc/cleanDatasets/")

# Used data frames
dataframes.names <- c("breast-cancer", "vote", "mushroom", 
                      "primary-tumor", "adult", "autism-adult", 
                      "credit-approval", "hcc-survival", "cylinder-bands",
                      "kick")
