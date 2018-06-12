
##############################
###        Mushroom        ###
##############################


# create dataframes
##############################
# setwd("D:/workspace/UFRN/R")    # run on windows
setwd("~/Workspace/UFRN/R")     # run on linux
##############################

# imports
##############################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
##############################

# create dataframes
##############################
dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[1], "")
dataframe.original <- pushClassToTheEnd(dataframe.original, 1, colnames(dataframe.original)[1])
dataframe.original <- dropLevelFromDataframe(dataframe.original, "NULO")

dataframe.noNA <- getCompleteCases(dataframe.original)
dataframe.onlyNA <- getIncompleteCases(dataframe.original)
##############################

# setting convertion parameters
##############################
convertion.types <- c(2, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2)
convertion.levels <- list(c("f", "t"),
                          c("d", "c", "w"),
                          c("n", "o", "t"),
                          c("y", "s", "c", "v", "n", "a"))
##############################

dataframe.final <- fillNAs(dataframe.noNA, dataframe.onlyNA)
writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModaConjuntoCompleto")