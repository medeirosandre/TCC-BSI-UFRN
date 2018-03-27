##############################################
###       Cleaning Credit Approval         ###
##############################################


source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")

dataFrame <- read.csv(paste(dataFramesLocation[1], 
                            dataFramesNames[3], 
                            ".csv", sep = ""), 
                      stringsAsFactors = TRUE)

cleanDataFrame <- data.frame(matrix(1, nrow = nrow(dataFrame), 1))

dataFrame$A1 <- droplevels(dataFrame$A1, exclude = "")

# dataFrame[[1]][is.na(dataFrame[[1]])] <- "teste"

# for(i in 1:length(dataFrame)-1)
# {
#   if(!is.numeric(dataFrame[[i]]))
#   {
#     cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNum(dataFrame, i))
#   }
# }

# cleanDataFrame <- cbind(cleanDataFrame, convertColCatToNum(dataFrame, 1))

# cleanDataFrame <- cleanDataFrame[,-1]
# cleanDataFrame$class <- dataFrame$class

# write.csv(cleanDataFrame, paste(dataFramesLocation[2],
#                                 dataFramesNames[3],
#                                 ".csv", sep = ""),
#           na = "", row.names = FALSE)
