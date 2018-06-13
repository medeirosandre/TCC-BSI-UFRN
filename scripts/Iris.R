
##############################
###          Iris          ###
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
dataframe.original <- readFromCsv(dataframes.location[1], dataframes.names[3], "")
###############################################################################

# df.converted <- dataframe.original
# 
# row <- df.converted[sample(1:nrow(df.converted), 1), ]
# # row <- df.converted[901, ]
# df.converted <- df.converted[-c(as.numeric(rownames(row)[1])), ]
# 
# nn <- findKNNOfARow(df.converted, row, 9)
# nearestNeighbors <- dataframe.original[nn, ]
# 
# print(as.numeric(rownames(row)[1]))
# print(nn)

df.1 <- dataframe.original[sample(1:nrow(dataframe.original), 100), ]
df.2 <- dataframe.original[-c(as.integer(rownames(df.1))), ]

# df.1 <- dataframe.original[1:100, ]
# df.2 <- dataframe.original[-c(as.integer(rownames(df.1))), ]

cRow <- df.2[sample(1:nrow(df.2)), ]

nn <- findKNNOfARow(df.1, cRow, 9)
nearestNeighbors <- dataframe.original[nn, ]
# nearestNeighbors <- df.1[nn, ]

print(as.numeric(rownames(cRow)[1]))
print(nn)