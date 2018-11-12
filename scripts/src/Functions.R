
source("scripts/src/FunctionsFillNA.R")
source("scripts/src/Utils.R")

#' @description Append a row into a dataframe.
#' @param df_original the dataframe in which the row must be appended.
#' @param row_to_append a row to append into a dataframe.
#' @return a dataframe with the row appended into.
appendRowIntoDataframe <- function(df_original, row_to_append)
{
  return(rbind(df_original, row_to_append))
}

#' @description Convert the type of data in a dataframe from categorical to
#' numerical.
#' @param df_to_convert The dataframe in which the type of data must be
#' converted.
#' @return A dataframe that contains the same data as the input, only
#' represented as it's original type.
convertCategoricalToNumerical <- function(df_to_convert)
{
  #' Iterates over the columns for the dataframe, except the class (last one).
  for (i in 1:(ncol(df_to_convert)-1))
  {
    df_to_convert[[i]] <- type.convert(df_to_convert[[i]])
  }
  
  rm(i)
  return(df_to_convert)
}

#' @description Convert categorical data into ordinal numerical data.
#' @param column_data Data from the column that must be converted.
#' @param column_levels Values from the column in the right order.
#' @param column_name Final name of the converted column.
#' @return A dataframe with one column, containing the converted data.
convertColumnFromCategoricalToNumericalOrdinal <- function(
  column_data, column_levels, column_name)
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
  
  rm(column_data, column_levels, column_name, var_aux, var_aux1, i, j)
  return(df_aux)
}

#' @description Binarize a column that contains categorical data.
#' @param df_to_convert Original dataframe from which the column must be
#' observed.
#' @param column Index for the column that must observed within the dataframe.
#' @param column_name Final name of the column that must be converted.
#' @param column_levels Contains the possible values for the column.
#' @return A dataframe representing the binarized data from the column.
convertColumnFromCategoricalToNumericalThroughBinarization <- function(
  df_to_convert, column, column_name, column_levels)
{
  df.aux <- df_to_convert
  var.aux <- df.aux[, column]
  
  var.aux1 <- unique(column_levels)
  
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
    var.aux2 = c(var.aux2, paste(column_name, var.aux1[i], sep = "_"))
  }
  colnames(df.aux1) <- var.aux2
  
  rm(i, j, df_to_convert, column, column_name, column_levels, df.aux, var.aux,
     var.aux1, var.aux2)
  return(df.aux1)
}

#' @description Drop a column from a dataframe
#' @param df_to_drop_column The dataframe from which the column must be
#' dropped.
#' @param column_to_drop The column that must be dropped from the dataframe.
dropColumnFromDataFrame <- function(df_to_drop_column, column_to_drop)
{
  return(df_to_drop_column[, -column_to_drop])
}

#' @description Drop a level from a dataframe.
#' @param df_to_drop_level Original dataframe from which the level must be
#' dropped.
#' @param level_to_drop Level to drop from the dataframe.
#' @return A dataframe with such level dropped.
dropLevelFromDataframe <- function(df_to_drop_level, level_to_drop="?")
{
  return(droplevels(df_to_drop_level, exclude = level_to_drop))
}

#' @description Find the fashion of a vector.
#' @param vector The vector from which the fashion must be observed.
#' @return The fashion of a vector
findFashion <- function(vector)
{
  return(names(which.max(table(vector))))
}

#' @description Find the k-nearest neighbors of an individual row within a
#' dataframe.
#' @param df_to_analize A dataframe in which the nearest neighbors must be
#' found.
#' @param row A dataframe containing the single row from which the NN must be
#' found.
#' @param num_of_k The number of nearest neighbors who must be found.
#' @return A dataframe contaning the k-nearest neighbors of the given row.
findKNNOfARow <- function(df_to_analize, row, num_of_k=1)
{
  target_values <- df_to_analize[[ncol(df_to_analize)]]
  df_training <- df_to_analize[, -ncol(df_to_analize)]
  df_test <- row[,-ncol(row)]
  
  k <- knn(df_training, df_test, target_values, k = num_of_k,
           algorithm = "cover_tree")
  indices <- attr(k, "nn.index")
  
  knn_num_indexes <- c(as.numeric(indices[1, ]))
  knn_row_names <- rownames(df_training)[knn_num_indexes]
  
  nearest_neighbors <- df_training[
    which(rownames(df_training) == knn_row_names[1]), ]
  for (i in 2:length(knn_row_names)) {
    nearest_neighbors <- appendRowIntoDataframe(
      df_original = nearest_neighbors,
      row_to_append = df_training[
        which(rownames(df_training) == knn_row_names[i]), ]
    )
  }
  
  rm(df_to_analize, row, num_of_k, target_values, df_training, df_test, k,
     indices, knn_num_indexes, knn_row_names, i)
  return(nearest_neighbors)
}

#' @description Find the mean of a vector.
#' @param vector The vector from which the mean must be observed.
#' @return The mean of a vector.
findMean <- function(vector)
{
  return(mean(as.numeric(vector)))
}

#' @description Find the original data for the NN.
#' @param df_to_analize The dataframe containing the original data.
#' @param df_neighbors The dataframe containing the data from the found NN.
#' @return A dataframe with the original data equivalent to the found NN.
findOriginalDataForNeighbors <- function(df_to_analize, df_neighbors)
{
  df_return <- df_to_analize[which(rownames(
    df_to_analize) == rownames(df_neighbors)[1]), ]
  for (i in 2:nrow(df_neighbors))
  {
    df_return <- rbind(df_return, df_to_analize[which(rownames(
      df_to_analize) == rownames(df_neighbors)[i]), ])
  }
  
  rm(df_to_analize, df_neighbors, i)
  return(df_return)
}

#' @description  Find which elements in a vector are NA.
#' @param vector A vector from which the NA must be found.
#' @return A vector with the indexes of the NA found whithin the analized
#' vector.
findWhichElementsInRowAreNA <- function(vector)
{
  return(which(is.na(vector)))
}

fixFinalDatasetKNN <- function(df_original)
{
  dataset_col <- df_original[, 1:2]
  df_return <- dropColumnFromDataFrame(df_original, 1)
  df_return <- fixDatasetKNN(df_return)
  
  df_return <- cbind(dataset_col, df_return)
  df_return <- dropColumnFromDataFrame(df_return, 2)
  
  rm(df_original, dataset_col)
  return(df_return)
}

fixDatasetKNN <- function(df_original)
{
  df_return <- df_original[, 1:9]
  knn_techniques_cols <- list(c(6:9), c(10:13), c(14:17), c(18:21))
  for(i in 1:nrow(df_original))
  {
    for(j in 1:length(knn_techniques_cols))
    {
      technique_mean <- mean(unname(
        unlist(df_original[i, c(knn_techniques_cols[[j]])])))
      df_return[i, 5 + j] <- technique_mean
    }
    rm(j, technique_mean)
  }
  rm(knn_techniques_cols, i, df_original)
  
  return(df_return)
}

#' @description Get all of the cases of a specific class.
#' @param df_to_be_observed Original dataframe from which the cases must be
#' observed.
#' @param class_name Value of the class who must be observed.
#' @return A dataframe containing only the cases of said specific class.
getCasesOfSpecificClass <- function(df_to_be_observed, class_name)
{
  return(df_to_be_observed[c(which(
    df_to_be_observed[[ncol(df_to_be_observed)]] == class_name)),])
}

#' @description Get a dataframe of complete cases from another dataframe.
#' @param df_to_get_cases Original dataframe from which the function finds the
#' complete cases.
#' @return A dataframe with the complete cases of the original dataframe.
getCompleteCases <- function(df_to_get_cases)
{
  return(df_to_get_cases[complete.cases(df_to_get_cases),])
}

#' @description Get a converted dataframe, using the convertion_types as a
#' method of differing the converting function.
#' @param df_to_convert Original dataframe that must be converted.
#' @param convertion_types List used to differt converting functions
#' 1 = convertion from ordinal categorical data to numerical data.
#' 2 = convertion from categorical data to numerical data through binarization.
#' 3 = just append.
#' @param column_levels List of vectors containing the levels for ordinal
#' convertion.
#' @return A dataframe containing the converted data.
getConvertedDataFrame <- function(
  df_to_convert, convertion_types, column_levels)
{
  df.aux <- data.frame(
    matrix(
      data = 1,
      nrow = nrow(df_to_convert),
      ncol = 1
    ),
    stringsAsFactors = F
  )
  
  var.aux1 <- 1
  for (i in 1:length(convertion_types)) {
    if (convertion_types[[i]][2] == 1)
    {
      df.aux <- cbind(
        df.aux,
        convertColumnFromCategoricalToNumericalOrdinal(
          column_data = df_to_convert[[i]], 
          column_levels = column_levels[[var.aux1]][-1], 
          column_name = colnames(df_to_convert)[i]
        )
      )
      var.aux1 <- var.aux1 + 1
    }
    else if (convertion_types[[i]][2] == 2)
    {
      df.aux <- cbind(
        df.aux,
        convertColumnFromCategoricalToNumericalThroughBinarization(
          df_to_convert = df_to_convert,
          column = i,
          column_name = colnames(df_to_convert)[i],
          column_levels = c(column_levels[[var.aux1]][-1])
        )
      )
      var.aux1 <- var.aux1 + 1
    }
    else if (convertion_types[[i]][2] == 3)
    {
      current_col <- data.frame(matrix(
        data = df_to_convert[, i],
        nrow = length(df_to_convert[, i]),
        ncol = 1
      ))
      colnames(current_col) <- colnames(df_to_convert)[i]
      
      df.aux <- cbind(df.aux, current_col)
    }
  }
  
  df.aux <- cbind(df.aux, df_to_convert[ncol(df_to_convert)])
  df.aux <- df.aux[, -1]
  
  rm(i, df_to_convert, convertion_types, column_levels, var.aux1)
  return(df.aux)
}

#' @description Get a dataframe of incomplete cases from another dataframe.
#' @param df_to_get_cases Original dataframe from which the function finds the
#' incomplete cases.
#' @return A dataframe with the incomplete cases of the orginal dataframe.
getIncompleteCases <- function(df_to_get_cases)
{
  return(df_to_get_cases[-c(match(rownames(
    getCompleteCases(df_to_get_cases)), rownames(df_to_get_cases))),])
}

#' @description Hide a column in a converted dataframe.
#' @param df_converted The converted dataframe.
#' @param columns_to_hide The indexes for the columns that must be hidden.
#' @param convrt_typs Represents the types of convertion that the dataframe
#' suffered.
#' @param convrt_lvls Represents the possible values for the original data in
#' the original categorical columns.
#' @param original_colnames The original names for the columns.
#' @return A dataframe containing the converted data, minus the columns which
#' were hidden.
hideColumnsOfConvertedDataframe <- function(
  df_converted, columns_to_hide, convrt_typs, convrt_lvls, original_colnames)
{
  colnames_to_hide <- c()
  for(i in columns_to_hide)
  {
    original_colname <- original_colnames[i]
    current_column_lvl <- which(lapply(convrt_lvls, `[[`,1) == i)
    current_column_typ <- which(lapply(convrt_typs, `[[`,1) == i)
    if(convrt_typs[[current_column_typ]][2] == 1
       || convrt_typs[[current_column_typ]][2] == 3)
    {
      colnames_to_hide <- c(colnames_to_hide, original_colname)
    }
    else if (convrt_typs[[current_column_typ]][2] == 2)
    {
      current_column_lvls <- convert_lvls[[current_column_lvl]][-1]
      for(j in current_column_lvls)
      {
        colnames_to_hide <- c(
          colnames_to_hide, paste(original_colname, j, sep = "_")
        )
      }
      
      rm(j)
    }
  }
  
  indexes_to_hide <- c()
  for(i in colnames_to_hide)
  {
    indexes_to_hide <- c(indexes_to_hide, which(colnames(df_converted) == i))
  }
  
  rm(i, columns_to_hide, convrt_typs, convrt_lvls, original_colnames)
  return(df_converted[, -c(indexes_to_hide)])
}

#' @description  Hide columns in a dataframe.
#' @param df_to_hide_columns A dataframe from which the columns must be hidden.
#' @param columns A vector of integers containing the indexes of the columns
#' who must be hidden.
#' @return The original dataframe without the hidden columns.
hideColumnsOfDataframe <- function(df_to_hide_columns, columns)
{
  return(df_to_hide_columns[, -columns])
}

#' @description Hide elements inside a list, using the first value of each 
#' element from the list to determine if said element must be hidden.
#' @param aList Represents the list from which the elemenst must be hidden.
#' @param elements_to_hide Contain the comparation values.
#' @return A list without the elements which were supposed the be hidden.
hideElementsInAList <- function(aList, elements_to_hide)
{
  auxList <- list()
  auxVar <- 1
  
  if(length(aList) >= 1)
  {
    for(i in 1:length(aList))
    {
      cElement <- aList[[i]]
      if(length(which(elements_to_hide == cElement[1])) == 0)
      {
        auxList[[auxVar]] <- cElement
        auxVar <- auxVar + 1
      }
    }
    rm(cElement, i)
  }
  
  rm(aList, elements_to_hide, auxVar)
  return(auxList)
}

#' @description Normalize specific columns in a dataframe.
#' @param df_to_normalize The dataframe that must be normalized.
#' @param columns_to_normalize Indexes for the columns that must be normalized.
#' @return The normalized dataframe.
normalizeColumnsOfDataframe <- function(df_to_normalize, columns_to_normalize)
{
  #' Iterate over the columns that must be normalized
  for(i in columns_to_normalize)
  {
    #' Retrieves the current column.
    col <- df_to_normalize[[i]]
    
    #' Identifies the indexes for NA in the column.
    na_col <- c()
    na_col <- which(is.na(col))
    
    #' Replaces NA with a value from the column, normalizes and inserts
    #' the NA back to it's places.
    col[na_col] <- col[which(!is.na(col))[1]]
    col <- (col - min(col)) / (max(col) - min(col))
    col[na_col] <- NA
    
    #' Replaces the column in the dataframe to be returned.
    df_to_normalize[[i]] <- col
    
    #' Removes disposable variables from enviroment
    rm(col, na_col)
  }
  
  return(df_to_normalize)
}

#' @description Normalize specific columns in a dataframe.
#' @param df_to_normalize The dataframe that must be normalized.
#' @param columns_to_normalize Indexes for the columns that must be normalized.
#' @return The normalized dataframe.
normalizeColumnsOfDataframeBasedOnOtherDataframe <- function(
  df_to_normalize, df_to_analize, columns_to_normalize)
{
  #' Iterate over the columns that must be normalized
  for(i in columns_to_normalize)
  {
    #' Retrieves the current column.
    col <- df_to_normalize[[i]]
    col_to_analize <- df_to_analize[[i]]
    
    #' Identifies the indexes for NA in the column.
    na_col <- c()
    na_col_analize <- c()
    na_col <- which(is.na(col))
    na_col_analize <- which(is.na(col_to_analize))
    
    #' Replace NA with a value from the column.
    col_to_analize[na_col_analize] <- 
      col_to_analize[which(!is.na(col_to_analize))[1]]
    
    #' Set min and max values.
    min <- min(col_to_analize)
    max <- max(col_to_analize)
    
    #' Replaces NA with a value from the column, normalizes and inserts
    #' the NA back to it's places.
    col[na_col] <- min
    col <- (col - min) / (max - min)
    col[na_col] <- NA
    
    #' Replaces the column in the dataframe to be returned.
    df_to_normalize[[i]] <- col
  }
  
  rm(df_to_analize, columns_to_normalize)
  return(df_to_normalize)
}

#' @description Normalize a dataframe.
#' @param df_to_normalize The dataframe that must be normalized.
#' @return The normalized dataframe.
normalizeDataframe <- function(df_to_normalize)
{
  #' Iterate over the dataframe's columns, except class (last one)
  for(i in 1:(ncol(df_to_normalize)-1))
  {
    #' Retrieves the current column.
    col <- df_to_normalize[[i]]
    
    #' Identifies the indexes for NA in the column.
    na_col <- c()
    na_col <- which(is.na(col))
    
    #' Replaces NA with a value from the column, normalizes and inserts
    #' the NA back to it's places.
    col[na_col] <- col[which(!is.na(col))[1]]
    col <- (col - min(col)) / (max(col) - min(col))
    col[na_col] <- NA
    
    #' Replaces the column in the dataframe to be returned.
    df_to_normalize[[i]] <- col
  }
  
  #' Removes disposable variables from enviroment
  rm(col, na_col)
  return(df_to_normalize)
}

#' @description Normalize a Dataframe based on the values of another dataframe.
#' @param df_to_normalize Dataframe that must be normalized.
#' @param df_to_analize Dataframe with the values that must be observed.
#' @return A dataframe with it's values normalized.
normalizeDataframeBasedOnOtherDataframe <- function(
  df_to_normalize, df_to_analize)
{
  #' Iterate over the dataframe's columns, except class (last one)
  for(i in 1:(ncol(df_to_normalize)-1))
  {
    #' Retrieves the current column.
    col <- df_to_normalize[[i]]
    col_to_analize <- df_to_analize[[i]]
    
    #' Identifies the indexes for NA in the column.
    na_col <- c()
    na_col_analize <- c()
    na_col <- which(is.na(col))
    na_col_analize <- which(is.na(col_to_analize))
    
    #' Replace NA with a value from the column.
    col_to_analize[na_col_analize] <- 
      col_to_analize[which(!is.na(col_to_analize))[1]]
    
    #' Set min and max values.
    min <- min(col_to_analize)
    max <- max(col_to_analize)
    
    #' Replaces NA with a value from the column, normalizes and inserts
    #' the NA back to it's places.
    col[na_col] <- min
    col <- (col - min) / (max - min)
    col[na_col] <- NA
    
    #' Replaces the column in the dataframe to be returned.
    df_to_normalize[[i]] <- col
  }
  
  #' Removes disposable variables from enviroment
  rm(col, na_col, col_to_analize, na_col_analize)
  return(df_to_normalize)
}

#' @description Move the target column of a dataframe to the end of said
#' dataframe.
#' @param df_to_move_colum A dataframe from which the column must be moved.
#' @param column Index for a column who must be moved within a dataframe.
#' @param columnName Final name of the moved column.
#' @return A dataframe with the target attribute as the last column.
pushClassToTheEnd <- function(df_to_move_colum, column=1, column_name="class")
{
  column_to_move <- df_to_move_colum[, column]
  df_to_move_colum <- df_to_move_colum[, -column]
  
  new_colnames <- c(colnames(df_to_move_colum), column_name)
  
  df_to_move_colum <- cbind(df_to_move_colum, column_to_move)
  colnames(df_to_move_colum) <- new_colnames
  
  rm(column_to_move, new_colnames, column, column_name)
  return(df_to_move_colum)
}

#' @description Finds all of the instances from a dataframe that doesn't have
#' any of it's features filled.
#' @param df_to_find_instances The dataframe from which the instances must be
#' found.
#' @return The instances that doesn't have any of it's features filled.
whichInstancesAreFullNA <- function(df_to_find_instances)
{
  rows_to_drop <- c()
  for(i in 1:nrow(df_to_find_instances))
  {
    if(length(which(is.na(
      df_to_find_instances[i,]))) == ncol(df_to_find_instances)-1)
    {
      rows_to_drop <- c(rows_to_drop, as.integer(
        rownames(df_to_find_instances[i, ])))
    }
  }
  
  rm(df_to_find_instances, i)
  return(rows_to_drop)
}
