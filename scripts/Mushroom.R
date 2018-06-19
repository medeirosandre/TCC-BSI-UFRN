
##############################
###        Mushroom        ###
##############################


# create dataframes
###############################################################################
# setwd("D:/workspace/UFRN/R")    # run on windows
setwd("~/Workspace/UFRN/R")     # run on linux
###############################################################################

# imports
###############################################################################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
###############################################################################

# create dataframes
###############################################################################
dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[1], "")
dataframe.original <- pushClassToTheEnd(dataframe.original, 1, colnames(dataframe.original)[1])
dataframe.original <- dropLevelFromDataframe(dataframe.original, "NULO")

dataframe.noNA <- getCompleteCases(dataframe.original)
dataframe.onlyNA <- getIncompleteCases(dataframe.original)
###############################################################################

# setting convertion parameters
###############################################################################
convertion.types <- list(c(1, 2), c(2, 2), c(3, 2), c(4, 1), c(5, 2), 
                         c(6, 2), c(7, 1), c(8, 2), c(9, 2), c(10, 2), 
                         c(11, 2), c(12, 2), c(13, 2), c(14, 2), c(15, 2), 
                         c(16, 2), c(17, 2), c(18, 1), c(19, 2), c(20, 2), 
                         c(21, 1), c(22, 2))
convertion.levels <- list(c(4, "f", "t"),
                          c(7, "d", "c", "w"),
                          c(18, "n", "o", "t"),
                          c(21, "y", "s", "c", "v", "n", "a"))
###############################################################################

# dataframe.final <- fillNAWithKNNFromCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, 9)

# converting dataframes
###############################################################################
# dataframe.final <- fillNAWithCompleteDatasetAppending(dataframe.noNA, dataframe.onlyNA)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModaConjuntoCompletoAppending")
# 
# dataframe.final <- fillNAWithCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModaConjuntoCompletoNotAppending")
# 
# dataframe.final <- fillNAWithDatasetOfCasesClassAppending(dataframe.noNA, dataframe.onlyNA)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModaConjuntoDeMesmaClasseAppending")
# 
# dataframe.final <- fillNAWithDatasetOfCasesClassNotAppending(dataframe.noNA, dataframe.onlyNA)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModaConjuntoDeMesmaClasseNotAppending")
###############################################################################