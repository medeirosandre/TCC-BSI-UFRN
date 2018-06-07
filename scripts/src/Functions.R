
##############################
###        Functions       ###
##############################

# Functions to manipulate csv files
###############################################################################

### <summary>
### funcion to load a csv file and return a dataframe
### </summary>
### <param name=df.location>integer, index for path to find dataframe</param>
### <param name=df.name>integer, index for the name of the dataframe</param>
### <return>returns a dataframe from csv file</return>
readFromCsv <- function(df.location, df.name)
{
  df.aux <- read.csv(paste(dataframes.location[df.location], 
                                       dataframes.names[df.name], 
                                       ".csv", sep = ""), 
                                 stringsAsFactors = TRUE)
  return(df.aux)
}

### <summary>
### funcion to write a dataframe into a csv file
### </summary>
### <param name=df.toWrite>dataframe, dataframe to be written into a csv file</param>
### <param name=df.location>integer, index for path in which a dataframe must be written</param>
### <param name=df.name>integer, index for the name of the dataframe</param>
writeToCsv <- function(df.toWrite, df.location, df.name)
{
  write.csv(df.toWrite, paste(dataframes.location[df.location], 
                              dataframes.names[df.name],".csv", sep = ""), 
            quote = FALSE, 
            na = "", row.names = FALSE)
}
###############################################################################

# Functions to manipulate a dataframe
###############################################################################

### <summary>
### funcion to move the target column of a dataframe to the end of said dataframe
### </summary>
### <param name=df.original>dataframe, dataframe from which the column must be moved</param>
### <param name=column>integer, index for a column who must be moved within a dataframe</param>
### <param name=columnName>string, final name of the moved column</param>
### <return>returns a dataframe with the target attribute as the last column</return>
pushClassToTheEnd <- function(df.original, column, columnName)
{
  df.aux <- df.original
  var.aux <- df.aux[, column]
  
  df.aux <- df.aux[, -column]
  
  var.aux1 <- c(colnames(df.aux), columnName)
  
  df.aux <- cbind(df.aux, var.aux)
  colnames(df.aux) <- var.aux1
  
  return(df.aux)
}

### <summary>
### funcion to drop a level from a dataframe
### </summary>
### <param name=df.orivinal>dataframe, original dataframe from which the level must be dropped</param>
### <param name=levelToDrop>string, name of the level to drop from the dataframe</param>
### <return>returns a dataframe with such level dropped</return>
dropLevelFromDataframe <- function(df.original, levelToDrop)
{
  df.aux <- df.original
  df.aux <- droplevels(df.aux, exclude = levelToDrop)
  
  return(df.aux)
}

### <summary>
### funcion to get a dataframe of complete cases from another dataframe
### </summary>
### <param name=df.original>dataframe, original dataframe from which the function finds the complete cases</param>
### <return>returns a dataframe with the complete cases of the original dataframe</return>
getCompleteCases <- function(df.original)
{
  df.aux <- df.original
  df.aux <- df.aux[complete.cases(df.aux),]
  
  return(df.aux)
}
###############################################################################

# Functions to find mean and fashion
###############################################################################

### <summary>
### funcion to find the mean of a column in a dataframe
### </summary>
### <param name=df.original>dataframe, dataframe from which the column must be observed</param>
### <param name=column>integer, index for the column within the dataframe</param>
### <return>returns number representing the mean of such column</return>
findMean <- function(df.original, column)
{
  var.aux <- df.original[,column]
  
  return(mean(var.aux))
}

### <summary>
### funcion to find the fashion of a column in a dataframe
### </summary>
### <param name=df.original>dataframe, dataframe from which the column must be observed</param>
### <param name=column>integer, index for the column within the dataframe</param>
### <return>returns number representing the fashion of such column</return>
findFashion <- function(df.original, column)
{
  var.aux <- df.original
  var.aux <- var.aux[, column]

  var.aux <- summary(var.aux)
  var.aux <- names(sort(var.aux, decreasing = T))

  return(var.aux[1])
}
###############################################################################

# Functions to convert categorical values to numerical values
###############################################################################

### <summary>
### function to binarize a column with categorical data
### <summary>
### <param name=df.original>dataframe, original dataframe from which the column must be observed</param>
### <param name=column>integer, index for the column who must be observed within the dataframe</param>
### <param name=columnName>string, final name of the column who must be converted</param>
convertColumnFromCategoricalToNumericalByBinarization <- function(df.original, column, columnName)
{
  df.aux <- df.original
  var.aux <- df.aux[, column]
  
  var.aux1 <- unique(var.aux)
}
###############################################################################



# 
# # Categorical to Numerical data
# convertColCatToNum <- function(dataFrame, column)
# {
#   if(is.numeric(dataFrame[[column]]))
#   {
#     lvls <- as.character(unique(dataFrame[[column]]))
#     
#   }
#   else
#   {
#     lvls <- levels(dataFrame[[column]])
#   }
#   auxDataFrame <- data.frame(matrix(lvls, 
#                                     nrow = nrow(dataFrame), 
#                                     ncol = length(lvls)), 
#                              stringsAsFactors = FALSE)
#   colnames(auxDataFrame) <- lvls
#   
#   for (j in 1:nrow(dataFrame))
#   {
#     for (k in lvls)
#     {
#       aux <- dataFrame[j, column]
#       if (aux == k)
#       {
#         auxDataFrame[j,k] <- as.numeric(1)
#       }
#       else
#       {
#         auxDataFrame[j,k] <- as.numeric(0)
#       }
#     }
#   }
#   
#   for (l in 1:length(lvls))
#   {
#     lvls[l] = paste(colnames(dataFrame)[column], "_", lvls[l])
#   }
#   colnames(auxDataFrame) <- lvls
#   
#   return(auxDataFrame)
# }
# 
# # Categorical to Numerical data (Ordinal)
# convertColCatToNumOrd <- function(dataFrame, column, levels)
# {
#   newDataFrame <- data.frame(matrix(1, 
#                                     nrow = nrow(dataFrame), 
#                                     ncol = 1))
#   colnames(newDataFrame) <- colnames(dataFrame)[column]
#   
#   for (i in 1:nrow(dataFrame))
#   {
#     for (j in 1:length(levels))
#     {
#       if (dataFrame[i, column] == levels[j])
#       {
#         newDataFrame[i, 1] <- j
#       }
#     }
#   }
# 
#   return(newDataFrame)
# }
# 
# 
# ##############################
# ##      Data Imputation     ##
# ##############################
# 
# # d.o = dataframe.original
# # d.c = dataframe.clean
# # d.t = dataframe.treated
# fillNAs <- function(d.o, d.c, d.t, num.k)
# {
#   lvls <- d.c[[length(d.c)]]
#   d.train <- d.c[,-length(d.c)]
#   
#   k <- knn(d.train, d.train, lvls, k=num.k, algorithm = "cover_tree")
#   indices <- attr(k, "nn.index")
#   
#   for(i in 1:nrow(d.o))
#   {
#     for(j in 1:(ncol(d.o)-1))
#     {
#       if(is.na(d.o[i,j]) || d.o[i,j] == "")
#       {
#         # print(i)
#         # print(j)
#         nearest.neighbors <- d.t[indices[i,-1],]
#         if(is.numeric(d.o[[j]]))
#         {
#           d.o[i,j] <- media(nearest.neighbors ,j)
#         }
#         else{
#           d.o[i,j] <- moda(d.t ,nearest.neighbors, j)
#         }
#         # print(d.o[i,j])
#       }
#     }
#   }
#   
#   return(d.o)
# }
