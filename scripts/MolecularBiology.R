
source("tcc/scripts/Imports.R")
source("tcc/scripts/Functions.R")

dataFrame <- read.csv(paste(dataFramesLocation[1], dataFramesNames[5], ".csv", sep = ""))

class <- dataFrame$class
dataFrame <- dataFrame[,-1]

dataFrame <- droplevels(dataFrame, exclude = "")

dataFrame <- convertCatToNum(dataFrame)

dataFrame <- fillNAs(dataFrame, 1, 10)

dataFrame <- cbind(dataFrame, class)

write.csv(dataFrame, paste(dataFramesLocation[2], dataFramesNames[5], ".csv", sep = ""), row.names = FALSE)
