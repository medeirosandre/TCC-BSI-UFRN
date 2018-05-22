
##############################
###        Mushroom        ###
##############################


# create dataframes
##############################
# setwd("D:/workspace/UFRN/R")
setwd("~/Workspace/UFRN/R")
##############################

# imports
##############################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
##############################

# create dataframes
##############################
dataframe.original <- read.csv(paste(dataframes.location[1], 
                                     dataframes.names[1], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original
##############################

dataframe.default <- dataframe.default[,-1]
dataframe.default$class <- dataframe.original$class

dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 1))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, as.numeric(as.factor(dataframe.default[[4]])))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 5))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 7))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 8))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 10))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 11))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 12))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 13))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 14))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 15))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 16))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 17))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 18))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 19))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 20))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 21))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 22))

dataframe.clean <- dataframe.clean[,-1]

# write dataFrame to file
##############################
write.csv(x, paste(dataframes.location[2],
                   dataframes.names[1],".csv", sep = ""),
          na = "", row.names = FALSE)
##############################
