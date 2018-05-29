
#############################
###     Heart Disease     ###
#############################


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
dataframe.original <- read.csv(paste(dataframes.location[1], 
                                     dataframes.names[2], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original
##############################

# dataframe.original <- droplevels(dataframe.original, exclude = "ZZZ")

for(i in 1:nrow(dataframe.default))
{
  for(j in 1:ncol(dataframe.default))
  {
    if(j == 4 || j == 5 || j == 7 || j == 8 || j == 11 || j == 12 || j == 13)
    {
      x <- levels(dataframe.default[[j]])
      x <- c(x, c("0"))
      levels(dataframe.default[[j]]) <- x
      
      if(dataframe.default[i, 4] == "ZZZ")
      {
        dataframe.default[i, 4] <- "0"
      }
      if(dataframe.default[i, 5] == "ZZZ")
      {
        dataframe.default[i, 5] <- "0"
      }
      if(dataframe.default[i, 7] == "ZZZ")
      {
        dataframe.default[i, 7] <- "0"
      }
      if(dataframe.default[i, 8] == "ZZZ")
      {
        dataframe.default[i, 8] <- "0"
      }
      if(dataframe.default[i, 11] == "ZZZ")
      {
        dataframe.default[i, 11] <- "0"
      }
      if(dataframe.default[i, 12] == "ZZZ")
      {
        dataframe.default[i, 12] <- "0"
      }
      if(dataframe.default[i, 13] == "ZZZ")
      {
        dataframe.default[i, 13] <- "0"
      }
    }
  }
}

dataframe.clean <- cbind(dataframe.clean, dataframe.default[[1]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[4]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[5]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- dataframe.clean[,-ncol(dataframe.clean)]
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[7]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[8]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- dataframe.clean[,-ncol(dataframe.clean)]
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[10]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[11]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[12]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[13]])

dataframe.clean <- dataframe.clean[, -1]
dataframe.clean$num <- dataframe.original$num

# write dataFrame to file
##############################
write.csv(dataframe.default, paste(dataframes.location[2],
                                 dataframes.names[2],"_1.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)
##############################
