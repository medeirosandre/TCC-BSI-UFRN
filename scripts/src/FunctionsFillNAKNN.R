
#' @description Fill <NA> from the original dataframe, based on the NN of every
#' case and, if necessary, increments the dataframe from which the NN are found
#' with each new complete row.
#'
#' @param df.noNA dataframe containig the complete cases of the original
#' dataframe.
#' @param df.onlyNA dataframe containing the incomplete cases of the original
#' dataframe.
#' @param convert.typs list containing vectors informing the index for every
#' column and the convertion that must be made in said columns.
#' @param convert.lvls list containing vectors informing the index for the
#' columns that must be converted from ordinal categorical data to numeric
#' data, and the apropriate levels for that column.
#' @param fillUsing represents the method to be used to input the missing value
#' 1 = mean, 2 = fashion.
#' @param numOfK the number of K for KNN.
#' @param appending identifies if the case with it's NA filled must be appended
#' to the dataframe from which the NN are found.
#'
#' @return a dataframe containing all of the <NA> filled.
fillNAWithKNNFromCompleteDataset <- function(df.noNA, df.onlyNA, 
 convert.typs, convert.lvls, fillUsing, numOfK, appending = T)
{
  convertedDF.noNA <- getConvertedDataFrame(
    df.original = df.noNA,
    convertion.types = convert.typs,
    column.levels = convert.lvls
  )
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    if(length(cRow.idxNA) >= length(cRow.origData) - 2)
    {
      df_aux_one_feature <- auxDF.return[runif(numOfK, 1, nrow(auxDF.return)),]
      cRow.origData <- fillNAInARow(
        df.toAnalize = df_aux_one_feature,
        row = cRow.origData,
        row.naElements = cRow.idxNA,
        fillUsing = fillUsing
      )
      
      rm(df_aux_one_feature)
    }
    else
    {
      auxList.convTyps <- hideElementsInAList(
        aList = convert.typs,
        elementsToHide = cRow.idxNA
      )
      auxList.convLvls <- hideElementsInAList(
        aList = convert.lvls,
        elementsToHide = cRow.idxNA
      )
      
      cRow.convData <- hideColumnsOfDataframe(
        df.original = cRow.origData,
        columns = cRow.idxNA
      )
      cRow.convData <- getConvertedDataFrame(
        df.original = cRow.convData,
        convertion.types = auxList.convTyps,
        column.levels = auxList.convLvls
      )
      cRow.convData <- convertCategoricalToNumerical(cRow.convData)
      
      convertedDF.hidden <- hideColumnsOfConvertedDataframe(
        df_converted = convertedDF.noNA,
        columns_to_hide = cRow.idxNA,
        convrt_typs = convert.typs,
        convrt_lvls = convert.lvls,
        original_colnames = c(colnames(df.noNA))
      )
      
      auxDF.knn <- findKNNOfARow(
        dfToAnalize = convertedDF.hidden,
        row = cRow.convData,
        kNum = numOfK
      )
      
      auxDF.knnOrigData <- findOriginalDataForNeighbors(
        df.original = auxDF.return,
        df.neighbors = auxDF.knn
      )
      
      cRow.origData <- fillNAInARow(
        df.toAnalize = auxDF.knnOrigData,
        row = cRow.origData,
        row.naElements = cRow.idxNA,
        fillUsing = fillUsing
      )
      
      rm(
        auxList.convTyps, auxList.convLvls, cRow.convData, convertedDF.hidden,
        auxDF.knn, auxDF.knnOrigData
      )
    }
    
    auxDF.return <- appendRowIntoDataframe(
      df_original = auxDF.return,
      row_to_append = cRow.origData
    )
    
    if(appending)
    {
      cRow.convData <- getConvertedDataFrame(
        df.original = cRow.origData,
        convertion.types = convert.typs,
        column.levels = convert.lvls
      )
      cRow.convData <- convertCategoricalToNumerical(cRow.convData)
      convertedDF.noNA <- appendRowIntoDataframe(
        df_original = convertedDF.noNA,
        row_to_append = cRow.convData
      )
    }
    
    rm(cRow.origData, cRow.idxNA)
  }
  
  auxDF.return <- orderDataframeByRowname(auxDF.return)
  
  rm(
    df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK,
    convertedDF.noNA
  )
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
fillNAWithKNNFromDatasetOfCasesClass <- function(df.noNA, df.onlyNA,
 convert.typs, convert.lvls, fillUsing, numOfK, appending = T)
{
  convertedDF.noNA <- getConvertedDataFrame(
    df.original = df.noNA,
    convertion.types = convert.typs,
    column.levels = convert.lvls
  )
  convertedDF.noNA <- convertCategoricalToNumerical(convertedDF.noNA)
  
  auxDF.return <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    cRow.origData <- df.onlyNA[i, ]
    
    cRow.idxNA <- findWhichElementsInRowAreNA(cRow.origData)
    
    if(length(cRow.idxNA) >= length(cRow.origData) - 2)
    {
      df_aux_one_feature <- auxDF.return[runif(numOfK, 1, nrow(auxDF.return)),]
      cRow.origData <- fillNAInARow(
        df.toAnalize = df_aux_one_feature,
        row = cRow.origData,
        row.naElements = cRow.idxNA,
        fillUsing = fillUsing
      )
      
      rm(df_aux_one_feature)
    }
    else
    {
      auxList.convTyps <- hideElementsInAList(
        aList = convert.typs,
        elementsToHide = cRow.idxNA
      )
      auxList.convLvls <- hideElementsInAList(
        aList = convert.lvls,
        elementsToHide = cRow.idxNA
      )
      
      cRow.convData <- hideColumnsOfDataframe(
        df.original = cRow.origData,
        columns = cRow.idxNA
      )
      cRow.convData <- getConvertedDataFrame(
        df.original = cRow.convData,
        convertion.types = auxList.convTyps,
        column.levels = auxList.convLvls
      )
      cRow.convData <- convertCategoricalToNumerical(cRow.convData)
      
      convertedDF.hidden <- hideColumnsOfConvertedDataframe(
        df_converted = convertedDF.noNA,
        columns_to_hide = cRow.idxNA,
        convrt_typs = convert.typs,
        convrt_lvls = convert.lvls,
        original_colnames = c(colnames(df.noNA))
      )
      convertedDF.hidden <- getCasesOfSpecificClass(
        df.original = convertedDF.hidden,
        className = cRow.origData[[ncol(cRow.origData)]]
      )
      
      auxDF.knn <- findKNNOfARow(
        dfToAnalize = convertedDF.hidden,
        row = cRow.convData,
        kNum = numOfK
      )
      auxDF.knnOrigData <- findOriginalDataForNeighbors(
        df.original = auxDF.return,
        df.neighbors = auxDF.knn
      )
      
      cRow.origData <- fillNAInARow(
        df.toAnalize = auxDF.knnOrigData,
        row = cRow.origData,
        row.naElements = cRow.idxNA,
        fillUsing = fillUsing
      )
      
      rm(
        auxList.convTyps, auxList.convLvls, cRow.convData, convertedDF.hidden,
        auxDF.knn, auxDF.knnOrigData
      )
    }
    
    auxDF.return <- appendRowIntoDataframe(
      df_original = auxDF.return,
      row_to_append = cRow.origData
    )
    
    if(appending)
    {
      cRow.convData <- getConvertedDataFrame(
        df.original = cRow.origData,
        convertion.types = convert.typs,
        column.levels = convert.lvls
      )
      cRow.convData <- convertCategoricalToNumerical(cRow.convData)
      convertedDF.noNA <- appendRowIntoDataframe(
        df_original = convertedDF.noNA,
        row_to_append = cRow.convData
      )
    }
    
    rm(cRow.origData, cRow.idxNA)
  }
  
  auxDF.return <- orderDataframeByRowname(auxDF.return)
  
  rm(
    df.noNA, df.onlyNA, convert.typs, convert.lvls, fillUsing, numOfK,
    convertedDF.noNA
  )
  return(auxDF.return)
}