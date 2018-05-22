##############################################
###       Cleaning Car Evaluation          ###
##############################################

setwd("d:/workspace/ufrn/r")

source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")

dataFrame <- read.csv(paste(dataFramesLocation[1], dataFramesNames[1], ".csv", sep = ""))

cleanDataFrame <- data.frame(matrix(1, nrow = nrow(dataFrame), 1))

cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 1, c("low", "med", "high", "vhigh")))
cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 2, c("low", "med", "high", "vhigh")))
cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 3, c("2", "3", "4", "5more")))
cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 4, c("2", "4", "more")))
cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 5, c("small", "med", "big")))
cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNumOrd(dataFrame, 6, c("low", "med", "high")))

cleanDataFrame <- cleanDataFrame[,-1]
cleanDataFrame$class <- dataFrame$class

write.csv(cleanDataFrame, paste(dataFramesLocation[2], 
                                      dataFramesNames[1], 
                                      ".csv", 
                                      sep = ""), 
          na = "", row.names = FALSE)
