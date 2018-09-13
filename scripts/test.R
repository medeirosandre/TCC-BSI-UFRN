#' Run on Windows
# setwd("D:/workspace/UFRN/R")
#' Run on Linux
setwd("~/Workspace/UFRN")

source("tcc/scripts/src/Functions.R")
source("tcc/scripts/src/Imports.R")

dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[1], "")
dataframe.original <- dropLevelFromDataframe(dataframe.original, "?")

dataframe.noNA <- getCompleteCases(dataframe.original)
