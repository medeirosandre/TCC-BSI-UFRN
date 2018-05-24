
##############################
###        Mushroom        ###
##############################


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
                                     dataframes.names[1], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original
##############################

dataframe.default <- dataframe.default[,-1]
dataframe.default$class <- dataframe.original$class
dataframe.original <- dataframe.original[,-1]
dataframe.original$class <- dataframe.default$class

dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 1))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 4))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 5))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 7))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 8))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 10))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 11))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 12))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 13))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 14))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 15))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 16))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 17))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 18))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 19))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 20))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 21))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 22))

dataframe.clean <- dataframe.clean[,-1]
dataframe.clean$class <- dataframe.original$class

write.csv(dataframe.clean, paste(dataframes.location[2],
                                 dataframes.names[1],"_1.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)

dataframe.original[[11]] <- droplevels(dataframe.original[[11]], exclude = "ZZZ")


dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))

dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 1))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 4))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 5))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 7))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 8))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 10))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 11))
dataframe.clean <- dataframe.clean[,-length(dataframe.clean)]
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 12))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 13))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 14))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 15))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 16))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 17))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 18))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 19))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 20))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 21))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 22))

# write dataFrame to file
##############################
write.csv(dataframe.clean, paste(dataframes.location[2],
                   dataframes.names[1],"2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)

dataframe.clean <- read.csv(paste(dataframes.location[2],
                                  dataframes.names[1],
                                  "2.csv", sep = ""),
                            stringsAsFactors = TRUE)
##############################

dataframe.default <- fillNAs(dataframe.original, dataframe.clean, dataframe.default, 10)


dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))

dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 1))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 2))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 3))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 4))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 5))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 7))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 8))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 10))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 11))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 12))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 13))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 14))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 15))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 16))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 17))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 18))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 19))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 20))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 21))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 22))


dataframe.clean <- dataframe.clean[,-1]
dataframe.clean$class <- dataframe.original$class

write.csv(dataframe.clean, paste(dataframes.location[2],
                                 dataframes.names[1],"_2.csv", sep = ""),
          quote = FALSE,
          na = "", row.names = FALSE)
