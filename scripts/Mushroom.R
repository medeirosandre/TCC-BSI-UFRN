
##############################
###        Mushroom        ###
##############################


# create dataframes
###############################################################################
# setwd("D:/workspace/UFRN/R")    # run on windows
setwd("~/Workspace/UFRN")     # run on linux
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

# setting dataframe parameters
###############################################################################
convertion.types <- list(c(1, 2), c(2, 2), c(3, 2), c(4, 1), c(5, 2), 
                         c(6, 2), c(7, 1), c(8, 2), c(9, 2), c(10, 2), 
                         c(11, 2), c(12, 2), c(13, 2), c(14, 2), c(15, 2), 
                         c(16, 2), c(17, 2), c(18, 1), c(19, 2), c(20, 2), 
                         c(21, 1), c(22, 2))
convertion.levels <- list(c(1, c(levels(dataframe.original[[1]]))),
                          c(2, c(levels(dataframe.original[[2]]))),
                          c(3, c(levels(dataframe.original[[3]]))),
                          c(4, "f", "t"),
                          c(5, c(levels(dataframe.original[[5]]))),
                          c(6, c(levels(dataframe.original[[6]]))),
                          c(7, "d", "c", "w"),
                          c(8, c(levels(dataframe.original[[8]]))),
                          c(9, c(levels(dataframe.original[[9]]))),
                          c(10, c(levels(dataframe.original[[10]]))),
                          c(11, c(levels(dataframe.original[[11]]))),
                          c(12, c(levels(dataframe.original[[12]]))),
                          c(13, c(levels(dataframe.original[[13]]))),
                          c(14, c(levels(dataframe.original[[14]]))),
                          c(15, c(levels(dataframe.original[[15]]))),
                          c(16, c(levels(dataframe.original[[16]]))),
                          c(17, c(levels(dataframe.original[[17]]))),
                          c(18, "n", "o", "t"),
                          c(19, c(levels(dataframe.original[[19]]))),
                          c(20, c(levels(dataframe.original[[20]]))),
                          c(21, "y", "s", "c", "v", "n", "a"),
                          c(21, c(levels(dataframe.original[[21]]))))
fillNAUsing <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
###############################################################################



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
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 3)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa3NNConjuntoCompletoAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 5)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa5NNConjuntoCompletoAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 7)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa7NNConjuntoCompletoAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 9)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa9NNConjuntoCompletoAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 3)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa3NNConjuntoCompletoNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 5)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa5NNConjuntoCompletoNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 7)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa7NNConjuntoCompletoNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromCompleteDatasetNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 9)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa9NNConjuntoCompletoNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 3)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa3NNConjuntoDeMesmaClasseAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 5)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa5NNConjuntoDeMesmaClasseAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 7)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa7NNConjuntoDeMesmaClasseAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 9)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MediaEModa9NNConjuntoDeMesmaClasseAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 3)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MedieEModa3NNConjuntoDeMesmaClasseNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 5)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MedieEModa5NNConjuntoDeMesmaClasseNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 7)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MedieEModa7NNConjuntoDeMesmaClasseNotAppending")
# 
# dataframe.final <- fillNAWithKNNFromDatasetOfCasesClassNotAppending(dataframe.noNA, dataframe.onlyNA, convertion.types, convertion.levels, fillNAUsing, 9)
# writeToCsv(dataframe.final, dataframes.location[2], dataframes.names[1], "_MedieEModa9NNConjuntoDeMesmaClasseNotAppending")
###############################################################################