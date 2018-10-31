
#' @description Fill missing values in a single row, based on a dataframe.
#' @param df.toAnalize dataframe from which the value to be used is obtained.
#' @param row a single row of a dataframe in which the missing values must be
#' filled.
#' @param row.naElements a vector containing the indexes for the columns that
#' contain missing values.
#' @param fillUsing represents the method to be used to input the missing value
#' (1 = mean, 2 = fashion).
#' @return a dataframe containing a sigle row with it's missing values filled.
fillNAInARow <- function(df.toAnalize, row, row.naElements, fillUsing)
{
  for (j in 1:length(row.naElements)) {
    if (fillUsing[row.naElements[j]] == 1)
    {
      value.toReplace <- findMean(df.toAnalize[[row.naElements[j]]])
    }
    else
    {
      value.toReplace <- findFashion(df.toAnalize[[row.naElements[j]]])
    }
    row[, row.naElements[j]] <- value.toReplace
  }
  
  return(row)
}

#' @description Fill <NA> from the original dataframe based on the mean and
#' fashion of the dataframe, and, if necessary, increases the dataframe used to
#' obtain these values with the newly filled row.
#' @param df_to_convert Original dataframe to be filled.
#' @param fill_using Indicates what is supposed to be used to fill the <NA>.
#' @param appending Indicates if the newly filled row is to be appended into
#' the dataframe used to obtain the mean and fashion.
#' @param same_class Indicates if the dataframe used to obtain the mean and
#' fashion is to be made of the examples that has the same class as the current
#' row.
#' @return A dataframe with the original <NA> values filled.
fillNAWithDataset <- function(
  df_to_convert, fill_using, appending = F, same_class = F)
{
  #' Separate complete dataset from incomplete dataset.
  df_orig_noNA <- getCompleteCases(df_to_convert)
  df_orig_onlyNA <- getIncompleteCases(df_to_convert)
  
  #' Set training dataset.
  df_train <- df_orig_noNA
  
  #' Start the return dataframe with the original complete cases.
  df_return <- df_orig_noNA
  #' Iterates over the incomplete dataset.
  for(i in 1:nrow(df_orig_onlyNA))
  {
    #' Select current row.
    current_row_orig <- df_orig_onlyNA[i,]
    
    #' Identifies the elements in the row that are NA.
    current_row_na_index <- findWhichElementsInRowAreNA(current_row_orig)
    
    #' Set a training dataset for the row
    df_train_current <- df_train
    
    #' Separates the new training dataset IF the techniques requires for the
    #' dataset used to analize to be made of only the instances of the same
    #' class as the current row.
    if(same_class)
    {
      df_train_current <- getCasesOfSpecificClass(
        df_to_be_observed = df_train_current,
        class_name = current_row_orig[[ncol(current_row_orig)]]
      )
    }
    
    #' Fill NA in a row based on the dataset selected for this row.
    current_row_orig <- fillNAInARow(
      df.toAnalize = df_train_current,
      row = current_row_orig,
      row.naElements = current_row_na_index,
      fillUsing = fill_using
    )
    
    #' Append the current row into the returnable dataset.
    df_return <- appendRowIntoDataframe(df_return, current_row_orig)
    
    #' Appends the current row into the training dataset IF the technique
    #' requires it.
    if(appending)
    {
      df_train <- appendRowIntoDataframe(df_train, current_row_orig)
    }
    
    #' Removes variables from the enviroment.
    rm(current_row_orig, current_row_na_index, df_train_current)
  }
  rm(i)
  
  #' Order the rows of the dataframe to be returned.
  df_return <- orderDataframeByRowname(df_return)
  
  #' Remove disposable variables.
  rm(
    df_to_convert, fill_using, appending, same_class, df_orig_noNA,
    df_orig_onlyNA, df_train
  )
  return(df_return)
}

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
#' @param same_class Identifies if the training dataset is to be composed of
#' cases with the same class of the training dataset.
#' @return a dataframe containing all of the <NA> filled.
fillNAWithKNN <- function(
  df_to_complete, convert_typs, convert_lvls, columns_to_normalize, fill_using,
  num_of_k, appending = F, same_class = F)
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
    # df_to_convert = df_to_complete[as.integer(rownames(df_orig_noNA)),],
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
      # df_test <- hideColumnsOfDataframe(current_row_orig, current_row_na_index)
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
      
      if(same_class)
      {
        #' Select instances with the same class as current row
        df_train_hidden <- getCasesOfSpecificClass(
          df_to_be_observed = df_train_hidden,
          class_name = current_row_orig[[ncol(current_row_orig)]]
        )
      }
      
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
      current_row_orig <- convertCategoricalToNumerical(current_row_orig)
      current_row_norm <- normalizeColumnsOfDataframeBasedOnOtherDataframe(
        df_to_normalize = current_row_orig,
        df_to_analize = df_to_complete,
        columns_to_normalize = columns_to_normalize
      )
      
      #' Convert the data in the current row.
      current_row_append <- getConvertedDataFrame(
        df_to_convert = current_row_norm,
        # df_to_convert = current_row_orig,
        convertion_types = convert_typs,
        column_levels = convert_lvls
      )
      current_row_append <- convertCategoricalToNumerical(current_row_append)
      
      #' Append the current row into the training set.
      df_train <- appendRowIntoDataframe(df_train, current_row_append)
      
      #' Remove disposable variable from enviroment.
      rm(current_row_append)
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
