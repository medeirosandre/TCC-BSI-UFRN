
##############################
###     Car Evaluation     ###
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
dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[2], "")
###############################################################################

# setting convertion parameters
###############################################################################
convertion.types <- c(1, 1, 1, 1, 1, 1)
convertion.levels <- list(c("low", "med", "high", "v-high"),
                          c("low", "med", "high", "v-high"),
                          c("2", "3", "4", "5-more"),
                          c("2", "4", "more"),
                          c("small", "med", "big"),
                          c("low", "med", "high"))
###############################################################################

# PROBLEM, the test dataset is appearing in the resulting nearest neighbors
# easy solution, for every case which returns itself as nearest neighbor, 
#   repeat knn with k+1 and remove the case from the return

df.converted <- getConvertedDataFrame(dataframe.original, convertion.types, convertion.levels)

row <- df.converted[sample(1:nrow(df.converted), 1), ]
# row <- df.converted[901, ]
df.converted <- df.converted[-c(as.numeric(rownames(row)[1])), ]

nn <- findKNNOfARow(df.converted, row, 9)
nearestNeighbors <- dataframe.original[nn, ]
print(as.numeric(rownames(row)[1]))
print(nn)