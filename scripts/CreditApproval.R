
##############################
###    Credit Approval     ###
##############################

# imports
##############################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
##############################

# create dataframes
##############################
dataFrame.original <- read.csv(paste(dataFramesLocation[1], 
                                     dataFramesNames[3], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataFrame.clean <- data.frame(matrix(1, nrow = nrow(dataFrame.original), 1))
dataFrame.default <- dataFrame.original
##############################

# removing NAs from dataFrame.default
##############################
levels(dataFrame.default[[1]]) <- list(A="a", B="b", Z="")
levels(dataFrame.default[[4]]) <- list(L="l", U="u", Y="y", Z="")
levels(dataFrame.default[[5]]) <- list(G="g", GG="gg", P="p", Z="")
levels(dataFrame.default[[6]]) <- list(AA="aa", C="c", CC="cc", 
                                       D="d", E="e", FF="ff", 
                                       I="i", J="j", K="k", 
                                       M="m", Q="q", R="r", 
                                       W="w", X="x", Z="")
levels(dataFrame.default[[7]]) <- list(BB="bb", DD="dd", FF="ff", 
                                       H="h", J="j", N="n", 
                                       O="o", V="v", L="z", 
                                       Z="")
for(i in 1:(ncol(dataFrame.default)-1))
{
  if(is.numeric(dataFrame.default[[i]]))
  {
    dataFrame.default[[i]][is.na(dataFrame.default[[i]])] <- 0
  }
}
##############################

# creating dataFrame.clean
##############################
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 1))
dataFrame.clean <- dataFrame.clean[, -length(dataFrame.clean)]
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 4))
dataFrame.clean <- dataFrame.clean[, -length(dataFrame.clean)]
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 5))
dataFrame.clean <- dataFrame.clean[, -length(dataFrame.clean)]
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 6))
dataFrame.clean <- dataFrame.clean[, -length(dataFrame.clean)]
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 7))
dataFrame.clean <- dataFrame.clean[, -length(dataFrame.clean)]

dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 9))
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 10))
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 12))
dataFrame.clean <- cbind(dataFrame.clean, convertColCatToNum(dataFrame.default, 13))

for(i in 1:ncol(dataFrame.clean))
{
  dataFrame.clean[[i]] <- as.numeric(dataFrame.clean[[i]])
}

for(i in 1:(ncol(dataFrame.default)-1))
{
  if(is.numeric(dataFrame.default[[i]]))
  {
    dataFrame.clean[length(dataFrame.clean)+1] <- scale(dataFrame.default[[i]])
  }
}
##############################

# retrieving class
##############################
dataFrame.clean <- dataFrame.clean[,-1]
dataFrame.clean[length(dataFrame.clean)+1] <- dataFrame.default$Class
##############################

# dataframe.clean <- fillNAs(dataFrame.original, dataFrame.clean, dataFrame.default, 10)

levels <- dataFrame.clean[[47]]
dataframe.train <- dataFrame.clean[,-47]

k <- knn(dataframe.train, dataframe.train, levels, k=10, algorithm = "cover_tree")
indices <- attr(k, "nn.index")

nearest.neighbors <- dataFrame.default[indices[207,-1],]

# print(statmod(nearest.neighbors[,1]))
# print(mean(nearest.neighbors[,2]))

# write dataFrame to file
##############################
# write.csv(dataFrame.clean, paste(dataFramesLocation[2], 
#                                  dataFramesNames[3],".csv", sep = ""), 
#           na = "", row.names = FALSE)
##############################
