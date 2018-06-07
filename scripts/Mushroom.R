
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
dataframe.original <- readFromCsv(1, 1)
dataframe.original <- pushClassToTheEnd(dataframe.original, 1, "class")
dataframe.original <- dropLevelFromDataframe(dataframe.original, "NULO")

dataframe.noNA <- getCompleteCases(dataframe.original)
##############################