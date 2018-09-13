
#' Functions to be used in the experiments

#' Append a row into a dataframe.
#' 
#' @param df_original the dataframe in which the row must be appended.
#' @param row_to_append a row to append into a dataframe.
#' 
#' @return a dataframe with the row appended into.
appendRowIntoDataframe <- function(df_original, row_to_append)
{
  return(rbind(df_original, row_to_append))
}

#' Convert the type of data in a dataframe from categorical to numerical.
#' 
#' @param df_to_convert the dataframe in which the type of data must be converted.
#' 
#' @return a dataframe that contains the same data as the input, only represented in numeric type.
convertCategoricalToNumerical <- function(df_to_convert)
{
  for (i in 1:(ncol(df_to_convert)-1)) {
    df_to_convert[[i]] <- as.numeric(df_to_convert[[i]])
  }
  
  return(df_to_convert)
}

#' Convert categorical data into ordinal numerical data.
#' 
#' @param column_data data from the column that must be converted.
#' @param column_levels values from the column in the right order.
#' @param column_name final name of the converted column.
#' 
#' @return a dataframe with one column, containing the converted data.
convertColumnFromCategoricalToNumericalOrdinal <- function(column_data, column_levels, column_name)
{
  var_aux <- column_data
  var_aux1 <- column_levels
  df_aux <- data.frame(matrix(1,
                              nrow = length(var_aux),
                              ncol = 1),
                       stringsAsFactors = F)
  colnames(df_aux) <- column_name
  
  for (i in 1:length(var_aux)) {
    for (j in 1:length(var_aux1)) {
      if (var_aux[i] == var_aux1[j])
      {
        df_aux[i, 1] <- j
      }
    }
  }
  
  return(df_aux)
}

#' Binarize a column that contains categorical data.
#' 
#' @param df.original original dataframe from which the column must be observed.
#' @param column index for the column that must observed within the dataframe.
#' @param columnName final name of the column that must be converted.
#' @param coolumnLevels contains the possible values for the column.
#' 
#' @return a dataframe representing the binarized data from the column.
convertColumnFromCategoricalToNumericalThroughBinarization <- function(df.original, column, columnName, columnLevels)
{
  df.aux <- df.original
  var.aux <- df.aux[, column]
  
  # var.aux1 <- unique(var.aux)
  var.aux1 <- unique(columnLevels)
  
  df.aux1 <- data.frame(matrix(var.aux1,
                               nrow = length(var.aux),
                               ncol = length(var.aux1)),
                        stringsAsFactors = F)
  colnames(df.aux1) <- var.aux1
  
  for (i in 1:length(var.aux))
  {
    for (j in 1:length(var.aux1))
    {
      if(is.na(var.aux[i]))
      {
        df.aux1[i,j] <- as.numeric(0)
      }
      else
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
  }
  
  var.aux2 <- c()
  for (i in 1:length(var.aux1))
  {
    var.aux2 = c(var.aux2, paste(columnName, "_", var.aux1[i]))
  }
  colnames(df.aux1) <- var.aux2
  
  return(df.aux1)
}

#' Drop a level from a dataframe.
#' 
#' @param df.original original dataframe from which the level must be dropped.
#' @param levelToDrop name of the level to drop from the dataframe.
#' 
#' @return a dataframe with such level dropped.
dropLevelFromDataframe <- function(df.original, levelToDrop)
{
  df.aux <- df.original
  df.aux <- droplevels(df.aux, exclude = levelToDrop)
  
  return(df.aux)
}

#' Load a .csv file from de HD.
#' 
#' @param df.location a path to a .csv file.
#' @param df.name the .csv filename.
#' @param df.sufix the sufix for the .csv file.
#' 
#' @return a dataframe with the data from the .csv file.
readFromCsv <- function(df.location, df.name, df.sufix)
{
  df.aux <- read.csv(paste(df.location,
                           df.name, 
                           df.sufix, 
                           ".csv", sep = ""), 
                     stringsAsFactors = TRUE)
  return(df.aux)
}

#' Write a dataframe into a .csv file.
#' 
#' @param df.toWrite a dataframe that is supposed to be written
#' into a .csv file.
#' @param df.location the path in which the .csv file must be written.
#' @param df.name the name of the .csv file.
#' @param df.sufix the sufix of the .csv file.
writeToCsv <- function(df.toWrite, df.location, df.name, df.sufix)
{
  write.csv(df.toWrite, paste(df.location, 
                              df.name,
                              df.sufix,
                              ".csv", sep = ""), 
            quote = FALSE, 
            na = "", row.names = FALSE)
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
### function to get a converted dataframe, using the convertion.types as a method of 
### differing the converting function
### <summary>
### <param name="df.original">dataframe, original dataframe who must be converted</param>
### <param name="convertion.types">
### list, used to differt converting functions
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
    # if (convertion.types[i] == 1)
    if (convertion.types[[i]][2] == 1)
    {
      # df.aux <- cbind(df.aux, convertColumnFromCategoricalToNumericalOrdinal(
      #   df.original[[i]], column.levels[[var.aux1]], colnames(df.original)[i]))
      df.aux <- cbind(df.aux, convertColumnFromCategoricalToNumericalOrdinal(
        df.original[[i]], column.levels[[var.aux1]][-1], colnames(df.original)[i]))
      # var.aux1 <- var.aux1 + 1
    }
    # else if (convertion.types[i] == 2)
    else if (convertion.types[[i]][2] == 2)
    {
      df.aux <- cbind(df.aux, convertColumnFromCategoricalToNumericalThroughBinarization(
        df.original, i, colnames(df.original)[i], c(column.levels[[var.aux1]][-1])))
    }
    var.aux1 <- var.aux1 + 1
  }
  
  df.aux <- cbind(df.aux, df.original[ncol(df.original)])
  df.aux <- df.aux[, -1]
  
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

### <summary>
### function to hide column in a converted dataframe
### </summary>
### <param name="df.converted">dataframe, dataframe containing the converted data</param>
### <param name="columnstToHide">vector, contains the indexes for the columnst which must be hidden</param>
### <param name="convrt.typs">list, represents the types of convertions that the dataframe suffered</param>
### <param name="convrt.lvls">list, represents the possible values for the original data</param>
### <param name="originalColnames">vector, contains the names of the original columns for the dataframe</param>
### <return>returns a dataframe containing the converted data, minus the columns which were supposed to be hidden</return>
hideColumnsOfConvertedDataframe <- function(df.converted, columnsToHide, convrt.typs, convrt.lvls, originalColnames)
{
  colnamesToHide <- c()
  for(i in 1:length(columnsToHide))
  {
    auxVar <- originalColnames[columnsToHide[i]]
    if(convrt.typs[[columnsToHide[i]]][2] == 1)
    {
      colnamesToHide <- c(colnamesToHide, auxVar)
    }
    else if(convrt.typs[[columnsToHide[i]]][2] == 2)
    {
      auxVar2 <- convrt.lvls[[columnsToHide[i]]][-1]
      for(j in 1:length(auxVar2)) 
      {
        colnamesToHide <- c(colnamesToHide, paste(auxVar, "_", auxVar2[j]))
      }
    }
  }
  
  indexesToHide <- c()
  for(i in 1:length(colnamesToHide))
  {
    indexesToHide <- c(indexesToHide, which(colnames(df.converted) == colnamesToHide[i]))
  }
  
  return(df.converted[, -c(indexesToHide)])
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

# Functions to manipulate lists
###############################################################################

### <summary>
### function to hide elements inside a list, using the first value of each 
### element from the list to determine if said element must be hidden
### </summary>
### <param name="aList">list, represents the list from which the elemenst must
### be hidden</param>
### <param name="elementsToHide">vector, contain the comparation values</param>
### <return>returns a list without the elements which were supposed the be 
### hidden</return>
hideElementsInAList <- function(aList, elementsToHide)
{
  auxList <- list()
  auxVar <- 1
  for(i in 1:length(aList))
  {
    cElement <- aList[[i]]
    if(length(which(elementsToHide == cElement[1])) == 0)
    {
      auxList[[auxVar]] <- cElement
      auxVar <- auxVar + 1
    }
  }
  
  return(auxList)
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
  return(mean(df.original[,column]))
}
###############################################################################

# functions to analize data
###############################################################################

findOriginalDataForNeighbors <- function(df.original, df.neighbors)
{
  df.aux <- df.original[which(rownames(df.original) == rownames(df.neighbors)[1]), ]
  for (i in 2:nrow(df.neighbors))
  {
    df.aux <- rbind(df.aux, df.original[which(rownames(df.original) == rownames(df.neighbors)[i]), ])
  }
  
  return(df.aux)
}

### <summary>
### function to find which elements in a row are NA
### </summary>
### <param name="row">vector, a row from the dataframe, from which the NA must be found</param>
### <return>returns a vector with the indexes of the NA found whithin the analized row</return>
findWhichElementsInRowAreNA <- function(row)
{
  return(which(is.na(row)))
}

### <summary>
### function to get all of the cases of a specific class
### <summary>
### <param name="df.original">dataframe, original dataframe from which the cases must be observed</param>
### <param name="className">string, value of the class who must be observed</param>
### <return>returns a dataframe containing only the cases of said specific class</return>
getCasesOfSpecificClass <- function(df.original, className)
{
  return(df.original[c(which(df.original[[ncol(df.original)]] == className)),])
}
###############################################################################

# functions to fill missing values
###############################################################################

### <summary>
### function to fill missing values in a single row, based on a dataframe
### <summary>
### <param name="df.toAnalize">dataframe, dataframe from which the value to be used is obtained</param>
### <param name="row">dataframe, single row of a dataframe in which the missing values must be filled</param>
### <param name="row.naElements">vector, vector containing the indexes for the columns that 
### contain missing values within the row</param>
### <param name="fillUsing">vector, represents the method to be used to input the missing value
### 1 = fill NA using mean
### 2 = fill NA using fashion
### </param>
### <return>returns a dataframe containing a single row with it's missing values filled</return>
fillNAInARow <- function(df.toAnalize, row, row.naElements, fillUsing)
{
  for (j in 1:length(row.naElements)) {
    # if (is.numeric(df.toAnalize[[row.naElements[j]]]))
    if (fillUsing[row.naElements[j]] == 1)
    {
      value.toReplace <- findMean(df.toAnalize, row.naElements[j])
    }
    else
    {
      value.toReplace <- findFashion(df.toAnalize, row.naElements[j])
    }
    row[, row.naElements[j]] <- value.toReplace
  }
  
  return(row)
}

### <summary>
### function to fill missing values from original dataframe based in the original complete cases of said dataframe
### plus, the rows with the missing values recently filled
### </summary
### <param name="df.noNA">dataframe, dataframe containing only complete cases of an original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing only incomplete cases of an original dataframe</param>
### <return>returns a dataframe representing the original data with the missing values filled</return>
fillNAWithCompleteDatasetAppending <- function(df.noNA, df.onlyNA)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    current.row <- fillNAInARow(df.aux, current.row, column.na)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  return(df.aux)
}

### <summary>
### function to fill missing values from original dataframe based in the original complete cases of said dataframe
### </summary
### <param name="df.noNA">dataframe, dataframe containing only complete cases of an original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing only incomplete cases of an original dataframe</param>
### <return>returns a dataframe representing the original data with the missing values filled</return>
fillNAWithCompleteDatasetNotAppending <- function(df.noNA, df.onlyNA)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    current.row <- fillNAInARow(df.noNA, current.row, column.na)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  return(df.aux)
}

### <summary>
### function to fill missing values from original dataframe based in the original complete cases of said dataframe
### plus, the rows with the missing values recently filled, having the same class as the case which must be filled
### </summary
### <param name="df.noNA">dataframe, dataframe containing only complete cases of an original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing only incomplete cases of an original dataframe</param>
### <return>returns a dataframe representing the original data with the missing values filled</return>
fillNAWithDatasetOfCasesClassAppending <- function(df.noNA, df.onlyNA)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    df.aux2 <- getCasesOfSpecificClass(df.aux, as.character(current.row[, ncol(current.row)]))
    
    current.row <- fillNAInARow(df.aux2, current.row, column.na)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  return(df.aux)
}

### <summary>
### function to fill missing values from original dataframe based in the original complete cases of said dataframe
### having the same class as the case which must be filled
### </summary
### <param name="df.noNA">dataframe, dataframe containing only complete cases of an original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing only incomplete cases of an original dataframe</param>
### <return>returns a dataframe representing the original data with the missing values filled</return>
fillNAWithDatasetOfCasesClassNotAppending <- function(df.noNA, df.onlyNA)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    df.aux2 <- getCasesOfSpecificClass(df.noNA, as.character(current.row[, ncol(current.row)]))
    
    current.row <- fillNAInARow(df.aux2, current.row, column.na)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  return(df.aux)
}

### <summary>
### function to fill missing values from the original dataframe, based on the nearest neighbors of every case
### and increments the dataframe from which the neighbors are found with each new complete row
### </summary>
### <param name="df.noNA">dataframe, dataframe containing the complete cases of the original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing the incomplete cases of the original dataframe</param>
### <param name="convert.typs">list, contains vectors informing the index for every column and the convertion 
### which must be made in said columns</param>
### <param name="convert.lvls">list, contains vectors informing the index for the columns which must be converted 
### from ordinal categorical data to numeric data, and the appropriate levels for that column</param>
### <param name="fillUsing">vector, represents the method to be used to input the missing value
### 1 = fill NA using mean
### 2 = fill NA using fashion
### </param>
### <param name="numOfK">integer, represents the number or neighbors for the knn function</param>
### <return>returns a dataframe containing all of the missing cases filled</return>
fillNAWithKNNFromCompleteDatasetAppending <- function(df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK)
{
  convertedDF.noNA <- getConvertedDataFrame(df.noNA, convert.typs, convert.lvls)
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    
    auxList.convTyps <- hideElementsInAList(convert.typs, cRow.idxNA)
    auxList.convLvls <- hideElementsInAList(convert.lvls, cRow.idxNA)
    
    cRow.convData <- hideColumnsOfDataframe(cRow.origData, cRow.idxNA)
    cRow.convData <- getConvertedDataFrame(cRow.convData, auxList.convTyps, auxList.convLvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    
    convertedDF.hidden <- hideColumnsOfConvertedDataframe(convertedDF.noNA, cRow.idxNA, convert.typs, convert.lvls, c(colnames(df.noNA)))
    
    auxDF.knn <- findKNNOfARow(convertedDF.hidden, cRow.convData, numOfK)
    auxDF.knnOrigData <- findOriginalDataForNeighbors(df.noNA, auxDF.knn)
    
    cRow.origData <- fillNAInARow(auxDF.knnOrigData, cRow.origData, cRow.idxNA, fillUsing)
    
    auxDF.return <- appendRowIntoDataframe(auxDF.return, cRow.origData)
    
    cRow.convData <- getConvertedDataFrame(cRow.origData, convert.typs, convert.lvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    convertedDF.noNA <- appendRowIntoDataframe(convertedDF.noNA, cRow.convData)
  }
  
  return(auxDF.return)
}

### <summary>
### function to fill missing values from the original dataframe, based on the nearest neighbors of every case
### </summary>
### <param name="df.noNA">dataframe, dataframe containing the complete cases of the original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing the incomplete cases of the original dataframe</param>
### <param name="convert.typs">list, contains vectors informing the index for every column and the convertion 
### which must be made in said columns</param>
### <param name="convert.lvls">list, contains vectors informing the index for the columns which must be converted 
### from ordinal categorical data to numeric data, and the appropriate levels for that column</param>
### <param name="fillUsing">vector, represents the method to be used to input the missing value
### 1 = fill NA using mean
### 2 = fill NA using fashion
### </param>
### <param name="numOfK">integer, represents the number or neighbors for the knn function</param>
### <return>returns a dataframe containing all of the missing cases filled</return>
fillNAWithKNNFromCompleteDatasetNotAppending <- function(df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK)
{
  convertedDF.noNA <- getConvertedDataFrame(df.noNA, convert.typs, convert.lvls)
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    
    auxList.convTyps <- hideElementsInAList(convert.typs, cRow.idxNA)
    auxList.convLvls <- hideElementsInAList(convert.lvls, cRow.idxNA)
    
    cRow.convData <- hideColumnsOfDataframe(cRow.origData, cRow.idxNA)
    cRow.convData <- getConvertedDataFrame(cRow.convData, auxList.convTyps, auxList.convLvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    
    convertedDF.hidden <- hideColumnsOfConvertedDataframe(convertedDF.noNA, cRow.idxNA, convert.typs, convert.lvls, c(colnames(df.noNA)))
    
    auxDF.knn <- findKNNOfARow(convertedDF.hidden, cRow.convData, numOfK)
    auxDF.knnOrigData <- findOriginalDataForNeighbors(df.noNA, auxDF.knn)
    
    cRow.origData <- fillNAInARow(auxDF.knnOrigData, cRow.origData, cRow.idxNA, fillUsing)
    
    auxDF.return <- appendRowIntoDataframe(auxDF.return, cRow.origData)
  }
  
  return(auxDF.return)
}

### <summary>
### function to fill missing values from the original dataframe, based on the nearest neighbors of every case
### having the same class as the row with the missing values and increments the dataframe from which the 
### neighbors are found with each new complete row
### </summary>
### <param name="df.noNA">dataframe, dataframe containing the complete cases of the original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing the incomplete cases of the original dataframe</param>
### <param name="convert.typs">list, contains vectors informing the index for every column and the convertion 
### which must be made in said columns</param>
### <param name="convert.lvls">list, contains vectors informing the index for the columns which must be converted 
### from ordinal categorical data to numeric data, and the appropriate levels for that column</param>
### <param name="fillUsing">vector, represents the method to be used to input the missing value
### 1 = fill NA using mean
### 2 = fill NA using fashion
### </param>
### <param name="numOfK">integer, represents the number or neighbors for the knn function</param>
### <return>returns a dataframe containing all of the missing cases filled</return>
fillNAWithKNNFromDatasetOfCasesClassAppending <- function(df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK)
{
  convertedDF.noNA <- getConvertedDataFrame(df.noNA, convert.typs, convert.lvls)
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    
    auxList.convTyps <- hideElementsInAList(convert.typs, cRow.idxNA)
    auxList.convLvls <- hideElementsInAList(convert.lvls, cRow.idxNA)
    
    cRow.convData <- hideColumnsOfDataframe(cRow.origData, cRow.idxNA)
    cRow.convData <- getConvertedDataFrame(cRow.convData, auxList.convTyps, auxList.convLvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    
    convertedDF.hidden <- hideColumnsOfConvertedDataframe(convertedDF.noNA, cRow.idxNA, convert.typs, convert.lvls, c(colnames(df.noNA)))
    convertedDF.hidden <- getCasesOfSpecificClass(convertedDF.hidden, cRow.origData[[ncol(cRow.origData)]])
    
    auxDF.knn <- findKNNOfARow(convertedDF.hidden, cRow.convData, numOfK)
    auxDF.knnOrigData <- findOriginalDataForNeighbors(df.noNA, auxDF.knn)
    
    cRow.origData <- fillNAInARow(auxDF.knnOrigData, cRow.origData, cRow.idxNA, fillUsing)
    
    auxDF.return <- appendRowIntoDataframe(auxDF.return, cRow.origData)
    
    cRow.convData <- getConvertedDataFrame(cRow.origData, convert.typs, convert.lvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    convertedDF.noNA <- appendRowIntoDataframe(convertedDF.noNA, cRow.convData)
  }
  
  return(auxDF.return)
}

### <summary>
### function to fill missing values from the original dataframe, based on the nearest neighbors of every case
### having the same class as the row with the missing values
### </summary>
### <param name="df.noNA">dataframe, dataframe containing the complete cases of the original dataframe</param>
### <param name="df.onlyNA">dataframe, dataframe containing the incomplete cases of the original dataframe</param>
### <param name="convert.typs">list, contains vectors informing the index for every column and the convertion 
### which must be made in said columns</param>
### <param name="convert.lvls">list, contains vectors informing the index for the columns which must be converted 
### from ordinal categorical data to numeric data, and the appropriate levels for that column</param>
### <param name="fillUsing">vector, represents the method to be used to input the missing value
### 1 = fill NA using mean
### 2 = fill NA using fashion
### </param>
### <param name="numOfK">integer, represents the number or neighbors for the knn function</param>
### <return>returns a dataframe containing all of the missing cases filled</return>
fillNAWithKNNFromDatasetOfCasesClassNotAppending <- function(df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK)
{
  convertedDF.noNA <- getConvertedDataFrame(df.noNA, convert.typs, convert.lvls)
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    
    auxList.convTyps <- hideElementsInAList(convert.typs, cRow.idxNA)
    auxList.convLvls <- hideElementsInAList(convert.lvls, cRow.idxNA)
    
    cRow.convData <- hideColumnsOfDataframe(cRow.origData, cRow.idxNA)
    cRow.convData <- getConvertedDataFrame(cRow.convData, auxList.convTyps, auxList.convLvls)
    cRow.convData <- convertCategoricalToNumerical(cRow.convData)
    
    convertedDF.hidden <- hideColumnsOfConvertedDataframe(convertedDF.noNA, cRow.idxNA, convert.typs, convert.lvls, c(colnames(df.noNA)))
    convertedDF.hidden <- getCasesOfSpecificClass(convertedDF.hidden, cRow.origData[[ncol(cRow.origData)]])
    
    auxDF.knn <- findKNNOfARow(convertedDF.hidden, cRow.convData, numOfK)
    auxDF.knnOrigData <- findOriginalDataForNeighbors(df.noNA, auxDF.knn)
    
    cRow.origData <- fillNAInARow(auxDF.knnOrigData, cRow.origData, cRow.idxNA, fillUsing)
    
    auxDF.return <- appendRowIntoDataframe(auxDF.return, cRow.origData)
  }
  
  return(auxDF.return)
}
###############################################################################

# functions to use k-nn
###############################################################################

### <summary>
### function to find the k-nearest neighbors of an individual row within a dataframe
### </summary>
### <param name="dfToAnalize">dataframe, dataframe in which the nearest neighbors must be found</param>
### <param name="row">dataframe, dataframe containing the single row from which the nearest neighbors must be found</param
### <param name="kNum">integer, number of nearest neighbors who must be found</param>
### <return>returns a dataframe contaning the k-nearest neighbors of the given row</param>
findKNNOfARow <- function(dfToAnalize, row, kNum)
{
  targetValues <- dfToAnalize[[ncol(dfToAnalize)]]
  dfTraining <- dfToAnalize[, -ncol(dfToAnalize)]
  dfTest <- row[,-ncol(row)]
  
  k <- knn(dfTraining, dfTest, targetValues, k = kNum, algorithm = "cover_tree")
  indices <- attr(k, "nn.index")
  
  knn.numericIndexes <- c(as.numeric(indices[1, ]))
  knn.rowNames <- rownames(dfTraining)[knn.numericIndexes]
  nearestNeighbors <- dfTraining[which(rownames(dfTraining) == knn.rowNames[1]), ]
  for (i in 2:length(knn.rowNames)) {
    nearestNeighbors <- appendRowIntoDataframe(nearestNeighbors, dfTraining[which(rownames(dfTraining) == knn.rowNames[i]), ])
  }
  
  return(nearestNeighbors)
}
###############################################################################