
##############################
###        Mushroom        ###
##############################

setwd("f:/workspace/ufrn/r")

# imports
##############################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
##############################

# create dataframes
##############################
dataframe.original <- read.csv(paste(dataframes.location[1], 
                                     dataframes.names[3], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original
##############################



# write dataFrame to file
##############################
# write.csv(x, paste(dataframes.location[2],
#                    dataframes.names[3],".csv", sep = ""),
#           na = "", row.names = FALSE)
##############################