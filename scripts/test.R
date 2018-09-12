# setwd("D:/workspace/UFRN/R")    # run on windows
setwd("~/Workspace/UFRN")     # run on linux

source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")

dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[9], "")
dataframe.original <- dropLevelFromDataframe(dataframe.original, "?")

dataframe.noNA <- getCompleteCases(dataframe.original)
