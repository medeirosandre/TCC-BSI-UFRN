
#' @description Fill <NA> from the original dataframe, based on the NN of every
#' case and, if necessary, increments the dataframe from which the NN are found
#' with each new complete row.
#' @param df_to_complete Dataframe to be completed.
#' @param convert_typs List containing vectors informing the index for every
#' column and the convertion that must be made in said columns.
#' @param convert_lvls List containing vectors informing the index for the
#' columns that must be converted from ordinal categorical data to numeric
#' data, and the apropriate levels for that column.
#' @param columns_to_normalize Represents the columns that must be normalized.
#' @param fill_using Represents the method to be used to input the missing value
#' 1 = mean, 2 = fashion.
#' @param num_of_k the number of K for KNN.
#' @param appending identifies if the case with it's NA filled must be appended
#' to the dataframe from which the NN are found.
#' @return a dataframe containing all of the <NA> filled.
fillNAWithKNNFromCompleteDataset <- function(
  df_to_complete, convert_typs, convert_lvls, columns_to_normalize, fill_using,
  num_of_k, appending = T)
{
  #' Normalize original dataset.
  df_orig_normalized <- normalizeColumnsOfDataframe(
    df_to_normalize = df_to_complete,
    columns_to_normalize = columns_to_normalize
  )
  
  #' Separate complete dataset from incomplete dataset.
  df_orig_noNA <- getCompleteCases(df_to_complete)
  df_orig_onlyNA <- getIncompleteCases(df_to_complete)
  
  #' Convert training dataset.
  df_train <- getConvertedDataFrame(
    df_to_convert = df_orig_normalized[as.integer(rownames(df_orig_noNA)),],
    convertion_types = convert_typs,
    column_levels = convert_lvls
  )
  df_train <- convertCategoricalToNumerical(df_train)
  
  #' Start the return dataframe with the original complete cases.
  df_return <- df_orig_noNA
  #' Iterates over the incomplete dataset.
  for(i in 1:nrow(df_orig_onlyNA))
  {
    #' Select current row, both original data and normalized data.
    current_row_orig <- df_orig_onlyNA[i,]
    current_row_norm <- df_orig_normalized[
      as.integer(rownames(current_row_orig)),]
    
    #' Identifies the elements in the current row that are NA.
    current_row_na_index <- findWhichElementsInRowAreNA(current_row_orig)
    
    #' IF the current row only has one or zero filled features, fill NA using
    #' random elements from the returnable dataset.
    if(length(current_row_na_index) >= length(current_row_orig) - 2)
    {
      #' Select a number of cases based on the value of num_of_k from the
      #' returnable dataset.
      df_one_feature <- df_return[runif(num_of_k, 1, nrow(df_return)),]
      
      #' Fill NA in the current row based on the random cases dataset.
      current_row_orig <- fillNAInARow(
        df.toAnalize = df_one_feature,
        row = current_row_orig,
        row.naElements = current_row_na_index,
        fillUsing = fill_using
      )
      
      #' Remove disposable variable from enviroment.
      rm(df_one_feature)
    }
    else
    {
      #' Hide elements in the lists for conversion types and levels.
      current_row_conv_typs <- hideElementsInAList(
        aList = convert_typs,
        elements_to_hide = current_row_na_index
      )
      current_row_conv_lvls <- hideElementsInAList(
        aList = convert_lvls,
        elements_to_hide = current_row_na_index
      )
      
      #' Hide columns with NA in the current row and converts the data.
      df_test <- hideColumnsOfDataframe(current_row_norm, current_row_na_index)
      df_test <- getConvertedDataFrame(
        df_to_convert = df_test,
        convertion_types = current_row_conv_typs,
        column_levels = current_row_conv_lvls
      )
      df_test <- convertCategoricalToNumerical(df_test)
      
      #' Hide columns in the converted dataframe
      df_train_hidden <- hideColumnsOfConvertedDataframe(
        df_converted = df_train,
        columns_to_hide = current_row_na_index,
        convrt_typs = convert_typs,
        convrt_lvls = convert_lvls,
        original_colnames = c(colnames(df_orig_noNA))
      )
      
      #' Find the KNN for the training set and their original data.
      current_row_nn <- findKNNOfARow(df_train_hidden, df_test, num_of_k)
      current_row_nn_orig <- findOriginalDataForNeighbors(
        df_to_analize = df_return,
        df_neighbors = current_row_nn
      )
      
      #' Fill NA in the row based on the found NN.
      current_row_orig <- fillNAInARow(
        df.toAnalize = current_row_nn_orig,
        row = current_row_orig,
        row.naElements = current_row_na_index,
        fillUsing = fill_using
      )
      
      #' Remove disposale variables from enviorment.
      rm(
        current_row_conv_typs, current_row_conv_lvls, df_test, df_train_hidden,
        current_row_nn, current_row_nn_orig
      )
    }
    
    #' Append the newly filled row into the returnable dataframe.
    df_return <- appendRowIntoDataframe(df_return, current_row_orig)
    
    #' IF the row is supposed to be appended into the training set.
    if(appending)
    {
      #' Convert the data in the current row.
      current_row_orig <- getConvertedDataFrame(
        df_to_convert = current_row_orig,
        convertion_types = convert_typs,
        column_levels = convert_lvls
      )
      current_row_orig <- convertCategoricalToNumerical(current_row_orig)
      
      #' Append the current row into the training set.
      df_train <- appendRowIntoDataframe(df_train, current_row_orig)
    }
    
    #' Remove disposable variables from enviorment
    rm(current_row_orig, current_row_norm, current_row_na_index)
  }
  rm(i)
  
  #' Order the rows of the dataframe to be returned.
  df_return <- orderDataframeByRowname(df_return)
  
  #' Remove disposable variables.
  rm(
    df_to_complete, convert_typs, convert_lvls, columns_to_normalize,
    fill_using, num_of_k, appending, df_orig_noNA, df_orig_onlyNA,
    df_orig_normalized, df_train
  )
  return(df_return)
}

#' @description  Fill <NA> from the original dataframe based on the NN of every
#' case having the same class as the row with the <NA> and increments the
#' dataframe from which the NN are found with each new complete row.
#' @param df.noNA dataframe containig the complete cases of the original
#' dataframe.
#' @param df.onlyNA dataframe containing the incomplete cases of the original
#' dataframe.
#' @param convert.typs list containing vectors informing the index for every
#' column and the convertion that must be made in said columns.
#' @param convert.lvls list containing vectors informing the index for the
#' columns that must be converted from ordinal categorical data to numeric data,
#' and the apropriate levels for that column.
#' @param fillUsing represents the method to be used to input the missing value
#' 1 = mean, 2 = fashion.
#' @param numOfK the number of K for KNN.
#' @param appending identifies if the case with it's NA filled must be appended
#' to the dataframe from which the NN are found.
#' @return a dataframe containing all of the <NA> filled.
fillNAWithKNNFromDatasetOfCasesClass <- function(df.noNA, df.onlyNA,
 convert.typs, convert.lvls, fillUsing, numOfK, appending = T)
{
  convertedDF.noNA <- getConvertedDataFrame(
    df_to_convert = df.noNA,
    convertion_types = convert.typs,
    column_levels = convert.lvls
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
        elements_to_hide = cRow.idxNA
      )
      auxList.convLvls <- hideElementsInAList(
        aList = convert.lvls,
        elements_to_hide = cRow.idxNA
      )
      
      cRow.convData <- hideColumnsOfDataframe(
        df_to_hide_columns = cRow.origData,
        columns = cRow.idxNA
      )
      cRow.convData <- getConvertedDataFrame(
        df_to_convert = cRow.convData,
        convertion_types = auxList.convTyps,
        column_levels = auxList.convLvls
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
        df_to_be_observed = convertedDF.hidden,
        class_name = cRow.origData[[ncol(cRow.origData)]]
      )
      
      auxDF.knn <- findKNNOfARow(
        df_to_analize = convertedDF.hidden,
        row = cRow.convData,
        num_of_k = numOfK
      )
      auxDF.knnOrigData <- findOriginalDataForNeighbors(
        df_to_analize = auxDF.return,
        df_neighbors = auxDF.knn
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
        df_to_convert = cRow.origData,
        convertion_types = convert.typs,
        column_levels = convert.lvls
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