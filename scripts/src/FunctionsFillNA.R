
#' Functions to fill <NA> in a dataframe.

#' Fill missing values in a single row, based on a dataframe.
#' 
#' @param df.toAnalize dataframe from which the value to be used is obtained.
#' @param row a single row of a dataframe in which the missing values must be filled.
#' @param row.naElements a vector containing the indexes for the columns that contain missing values.
#' @param fillUsing represents the method to be used to input the missing value (1 = mean, 2 = fashion).
#' 
#' @return a dataframe containing a sigle row with it's missing values filled.
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

#' Fill <NA> from original dataframe based in the original complete cases of said dataframe, 
#' plus, the rows with <NA>  recently filled.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an original dataframe.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
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

#' Fill <NA> from original dataframe based in the original complete cases of said dataframe.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an original dataframe.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
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

#' Fill <NA> from the original dataframe based in the original complete cases of said datafame,
#' plus, the rows with <NA> recently filled, having the same classes as the case which must be filled.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an original dataframe.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
fillNAWithDatasetOfCasesClassAppending <- function(df.noNA, df.onlyNA)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) 
  {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    df.aux2 <- getCasesOfSpecificClass(df.aux, as.character(current.row[, ncol(current.row)]))
    
    current.row <- fillNAInARow(df.aux2, current.row, column.na)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  return(df.aux)
}

#' Fill <NA> from original dataframe based in the original complete cases of said dataframe having
#' the same class as the case which must be filled.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an original dataframe.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
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

#' Fill <NA> from the original dataframe, based on the NN of every case and increments the dataframe
#' from which the NN are found with each new complete row.
#' 
#' @param df.noNA dataframe containig the complete cases of the original dataframe.
#' @param df.onlyNA dataframe containing the incomplete cases of the original dataframe.
#' @param convert.typs list containing vectors informing the index for every column and the convertion
#' that must be made in said columns.
#' @param convert.lvls list containing vectors informing the index for the columns that must be converted
#' from ordinal categorical data to numeric data, and the apropriate levels for that column.
#' @param fillUsing represents the method to be used to input the missing value (1 = mean, 2 = fashion).
#' @param numOfK the number of K for KNN.
#' 
#' @return a dataframe containing all of the <NA> filled.
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

#' Fill <NA> from the original dataframe based on the NN of every case.
#' 
#' @param df.noNA dataframe containig the complete cases of the original dataframe.
#' @param df.onlyNA dataframe containing the incomplete cases of the original dataframe.
#' @param convert.typs list containing vectors informing the index for every column and the convertion
#' that must be made in said columns.
#' @param convert.lvls list containing vectors informing the index for the columns that must be converted
#' from ordinal categorical data to numeric data, and the apropriate levels for that column.
#' @param fillUsing represents the method to be used to input the missing value (1 = mean, 2 = fashion).
#' @param numOfK the number of K for KNN.
#' 
#' @return a dataframe containing all of the <NA> filled.
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

#' Fill <NA> from the original dataframe based on the NN of every case having the same class as the row
#' with the <NA> and increments the dataframe from which the NN are found with each new complete row.
#' 
#' @param df.noNA dataframe containig the complete cases of the original dataframe.
#' @param df.onlyNA dataframe containing the incomplete cases of the original dataframe.
#' @param convert.typs list containing vectors informing the index for every column and the convertion
#' that must be made in said columns.
#' @param convert.lvls list containing vectors informing the index for the columns that must be converted
#' from ordinal categorical data to numeric data, and the apropriate levels for that column.
#' @param fillUsing represents the method to be used to input the missing value (1 = mean, 2 = fashion).
#' @param numOfK the number of K for KNN.
#' 
#' @return a dataframe containing all of the <NA> filled.
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