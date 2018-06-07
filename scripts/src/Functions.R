
##############################
###        Functions       ###
##############################

# Functions to manipulate csv files
###############################################################################

### <summary>
### funcion to load a csv file and return a dataframe
### </summary>
### <param name="df.location">integer, index for path to find dataframe</param>
### <param name="df.name">integer, index for the name of the dataframe</param>
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
### <param name="df.toWrite">dataframe, dataframe to be written into a csv file</param>
### <param name="df.location">integer, index for path in which a dataframe must be written</param>
### <param name="df.name">integer, index for the name of the dataframe</param>
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
### function to append a row into a dataframe
### </summary>
### <param name="df.original">dataframa, original dataframe in which the row must be appended</param>
### <param name="rowToAppend">vector, a row to append into the dataframe</param>
### <return>returns a dataframe with the appended row</return>
appendRowIntoDataframe <- function(df.original, rowToAppend)
{
  return(rbind(df.original, rowToAppend))
}

### <summary>
### funcion to drop a level from a dataframe
### </summary>
### <param name="df.orivinal">dataframe, original dataframe from which the level must be dropped</param>
### <param name="levelToDrop">string, name of the level to drop from the dataframe</param>
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
### <param name="df.original">dataframe, original dataframe from which the function finds the complete cases</param>
### <return>returns a dataframe with the complete cases of the original dataframe</return>
getCompleteCases <- function(df.original)
{
  df.aux <- df.original
  df.aux <- df.aux[complete.cases(df.aux),]
  
  return(df.aux)
}

### <summary>
### function to get a dataframe of incomplete cases from another dataframe
### </summary>
### <param name="df.original">dataframe, original dataframe from which the function finds the incomplete cases</param>
### <return>returns a dataframe with the incomplete cases of the orginal dataframe</return>
getIncompleteCases <- function(df.original)
{
  return(df.original[-c(as.integer(rownames(getCompleteCases(df.original)))),])
}

### <summary>
### funcion to move the target column of a dataframe to the end of said dataframe
### </summary>
### <param name="df.original">dataframe, dataframe from which the column must be moved</param>
### <param name="column">integer, index for a column who must be moved within a dataframe</param>
### <param name="columnName">string, final name of the moved column</param>
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
###############################################################################

# Functions to find mean and fashion
###############################################################################

### <summary>
### funcion to find the fashion of a column in a dataframe
### </summary>
### <param name='df.original">dataframe, dataframe from which the column must be observed</param>
### <param name="column">integer, index for the column within the dataframe</param>
### <return>returns number representing the fashion of such column</return>
findFashion <- function(df.original, column)
{
  var.aux <- df.original
  var.aux <- var.aux[, column]
  
  var.aux <- summary(var.aux)
  var.aux <- names(sort(var.aux, decreasing = T))
  
  return(var.aux[1])
}

### <summary>
### funcion to find the mean of a column in a dataframe
### </summary>
### <param name="df.original">dataframe, dataframe from which the column must be observed</param>
### <param name="column">integer, index for the column within the dataframe</param>
### <return>returns number representing the mean of such column</return>
findMean <- function(df.original, column)
{
  var.aux <- df.original[,column]
  
  return(mean(var.aux))
}
###############################################################################

# Functions to convert categorical values to numerical values
###############################################################################

### <summary>
### function to convert categorical data into ordinal numerical data
### </summary>
### <param name="column.data">vector, data from the column which must be converted</param>
### <param name="column.levels">vector, values from the column in the right order</param>
### <param name="column.name">string, final name of the converted column</param>
### <return>returns a dataframe with one column, containing the converted data</return>
convertColumnFromCategoricalToNumericalOrdinal <- function(column.data, column.levels, column.name)
{
  var.aux <- column.data
  var.aux1 <- column.levels
  df.aux <- data.frame(matrix(1,
                              nrow = length(var.aux),
                              ncol = 1),
                       stringsAsFactors = F)
  colnames(df.aux) <- column.name
  
  for (i in 1:length(var.aux)) {
    for (j in 1:length(var.aux1)) {
      if (var.aux[i] == var.aux1[j])
      {
        df.aux[i, 1] <- j
      }
    }
  }
  
  return(df.aux)
}

### <summary>
### function to binarize a column with categorical data
### <summary>
### <param name="df.original">dataframe, original dataframe from which the column must be observed</param>
### <param name="column">integer, index for the column who must be observed within the dataframe</param>
### <param name="columnName">string, final name of the column who must be converted</param>
### <return>
### returns a dataframe with the same number of columns as the possible values of the column 
### to be converted, this dataframe represents the binarized data from such column
### </return>
convertColumnFromCategoricalToNumericalThroughBinarization <- function(df.original, column, columnName)
{
  df.aux <- df.original
  var.aux <- df.aux[, column]
  
  var.aux1 <- unique(var.aux)
  
  df.aux1 <- data.frame(matrix(var.aux1,
                               nrow = length(var.aux),
                               ncol = length(var.aux1)),
                        stringsAsFactors = F)
  colnames(df.aux1) <- var.aux1
  
  for (i in 1:length(var.aux))
  {
    for (j in 1:length(var.aux1))
    {
      if (var.aux[i] == var.aux1[j])
      {
        df.aux1[i,j] <- as.numeric(1)
      }
      else
      {
        df.aux1[i,j] <- as.numeric(0)
      }
    }
  }
  
  var.aux2 <- c()
  for (i in 1:length(var.aux1))
  {
    var.aux2 = c(var.aux2, paste(columnName, "_", var.aux1[i]))
  }
  colnames(df.aux1) <- var.aux2
  
  return(df.aux1)
}
###############################################################################

# functions to manipulate dataframes
###############################################################################

### <summary>
### function to get a converted dataframe, using the convertion.types as a method of 
### differing the converting function
### <summary>
### <param name="df.original">dataframe, original dataframe who must be converted</param>
### <param name="convertion.types">
### vector, used to differt converting functions
### 1 = convertion from ordinal categorical data to numerical data
### 2 = convertion from categorical data to numerical data through binarization
### </param>
### <param names="column.levels">list, list vectors containing the levels for ordinal convertion</param>
### <return>returns a dataframe containing the converted data</return>
getConvertedDataFrame <- function(df.original, convertion.types, column.levels)
{
  df.aux <- data.frame(matrix(1,
                              nrow = nrow(df.original),
                              ncol = 1),
                       stringsAsFactors = F)
  
  var.aux1 <- 1
  for (i in 1:length(convertion.types)) {
    if (convertion.types[i] == 1)
    {
      df.aux <- cbind(df.aux, convertColumnFromCategoricalToNumericalOrdinal(
        df.original[[i]], column.levels[[var.aux1]], colnames(df.original)[i]))
      var.aux1 <- var.aux1 + 1
    }
    else if (convertion.types[i] == 2)
    {
      df.aux <- cbind(df.aux, convertColumnFromCategoricalToNumericalThroughBinarization(
        df.original, i, colnames(df.original)[i]))
    }
  }
  
  df.aux <- cbind(df.aux, df.original[ncol(df.original)])
  df.aux <- df.aux[, -1]
  
  return(df.aux)
}

### <summary>
### function to hide columns in a dataframe
### </summary>
### <param name="df.original">dataframe, dataframe from which the columns must be hidden</param>
### <param name"columns">
### vector, vector of integers containing the indexes of the columns who must be hidden
### </param>
### <return>returns the original dataframe without the hidden columns</return>
hideColumnsOfDataframe <- function(df.original, columns)
{
  return(df.original[, -columns])
}
###############################################################################

# functions to analize data
###############################################################################

### <summary>
### function to find which elements in a row are NA
### </summary>
### <param name="row">vector, a row from the dataframe, from which the NA must be found</param>
### <return>returns a vector with the indexes of the NA found whithin the analized row</return>
findWhichElementsInRowAreNA <- function(row)
{
  return(which(is.na(row)))
}
###############################################################################