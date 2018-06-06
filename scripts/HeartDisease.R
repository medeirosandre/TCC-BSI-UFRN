
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

# Original dataframe
##############################
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[1]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[4]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[5]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[7]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[8]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[10]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[11]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[12]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[13]])

new.levels <- c("age", "sex_1", "sex_2", "cp_1", "cp_2", "cp_3", "cp_4", "trestbps", "chol", "fbs_1", "fbs_2", "fbs_nulo", "restecg", "thalach", "exang_1", "exang_2", "exang_nulo", "oldpeak", "slope", "ca", "thal")

dataframe.clean <- dataframe.clean[, -1]
colnames(dataframe.clean) <- new.levels
dataframe.clean$num <- dataframe.original$num

dataframe.clean <- droplevels(dataframe.clean, exclude = "ZZZ")

write.csv(dataframe.clean, paste(dataframes.location[2],
                                 dataframes.names[2],"_1.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)
##############################

# Clean dataframe
##############################
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original

for(i in 1:nrow(dataframe.default))
{
  for(j in 1:ncol(dataframe.default))
  {
    if(j == 4 || j == 5 || j == 7 || j == 8 || j == 11 || j == 12 || j == 13)
    {
      x <- levels(dataframe.default[[j]])
      x <- c(x, c("0"))
      levels(dataframe.default[[j]]) <- x

      if(dataframe.default[i, j] == "ZZZ")
      {
        dataframe.default[i, j] <- "0"
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

new.levels <- c("age", "sex_1", "sex_2", "cp_1", "cp_2", "cp_3", "cp_4", "trestbps", "chol", "fbs_1", "fbs_2", "restecg", "thalach", "exang_1", "exang_2", "oldpeak", "slope", "ca", "thal")

dataframe.clean <- dataframe.clean[, -1]
colnames(dataframe.clean) <- new.levels
dataframe.clean$num <- dataframe.original$num

for(i in 1:nrow(dataframe.default))
{
  for(j in 1:ncol(dataframe.default))
  {
    if(j == 6 || j == 9)
    {
      x <- levels(dataframe.default[[j]])
      x <- c(x, c("0"))
      levels(dataframe.default[[j]]) <- x
      
      if(dataframe.default[i, j] == "ZZZ")
      {
        dataframe.default[i, j] <- "0"
      }
    }
  }
}

dataframe.original <- droplevels(dataframe.original, exclude = "ZZZ")

write.csv(dataframe.clean, paste(dataframes.location[2],
                                 dataframes.names[2],"_2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)

dataframe.clean <- read.csv(paste(dataframes.location[2], 
                                     dataframes.names[2], 
                                     "_2.csv", sep = ""), 
                               stringsAsFactors = TRUE)

write.csv(dataframe.original, paste(dataframes.location[2],
                                 dataframes.names[2],"_2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)

dataframe.original <- read.csv(paste(dataframes.location[2], 
                                  dataframes.names[2], 
                                  "_2.csv", sep = ""), 
                            stringsAsFactors = TRUE)

write.csv(dataframe.default, paste(dataframes.location[2],
                                 dataframes.names[2],"_2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)

dataframe.default <- read.csv(paste(dataframes.location[2], 
                                  dataframes.names[2], 
                                  "_2.csv", sep = ""), 
                            stringsAsFactors = TRUE)

dataframe.default <- fillNAs(dataframe.original, dataframe.clean, dataframe.default, 10)

dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))

dataframe.clean <- cbind(dataframe.clean, dataframe.default[[1]])
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[4]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[5]])
# media de 1 e 0 PROBLEMA
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[7]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[8]])
# media de 1 e 0 PROBLEMA
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[10]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[11]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[12]])
dataframe.clean <- cbind(dataframe.clean, dataframe.default[[13]])

dataframe.clean <- dataframe.clean[, -1]
dataframe.clean$num <- dataframe.original$num

write.csv(dataframe.clean, paste(dataframes.location[2],
                                 dataframes.names[2],"_2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)