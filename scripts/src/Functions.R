
source("scripts/src/FunctionsFillNA.R")
source("scripts/src/FunctionsFillNAKNN.R")
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
  
  return(df_to_convert)
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
     indices, knn_num_indexes, knn_row_names)
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
  
  rm(df_to_analize, df_neighbors)
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

#' @description Get a dataframe of incomplete cases from another dataframe.
#' @param df_to_get_cases Original dataframe from which the function finds the
#' incomplete cases.
#' @return A dataframe with the incomplete cases of the orginal dataframe.
getIncompleteCases <- function(df_to_get_cases)
{
  return(df_to_get_cases[-c(match(rownames(
    getCompleteCases(df_to_get_cases)), rownames(df_to_get_cases))),])
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
  
  rm(df_to_find_instances)
  return(rows_to_drop)
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
    var.aux2 = c(var.aux2, paste(columnName, var.aux1[i], sep = "_"))
  }
  colnames(df.aux1) <- var.aux2
  
  return(df.aux1)
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
# 3 = just append
### </param>
### <param names="column.levels">list, list vectors containing the levels for ordinal convertion</param>
### <return>returns a dataframe containing the converted data</return>
getConvertedDataFrame <- function(df.original, convertion.types, column.levels)
{
  df.aux <- data.frame(
    matrix(
      data = 1,
      nrow = nrow(df.original),
      ncol = 1
    ),
    stringsAsFactors = F
  )
  
  var.aux1 <- 1
  for (i in 1:length(convertion.types)) {
    if (convertion.types[[i]][2] == 1)
    {
      df.aux <- cbind(
        df.aux,
        convertColumnFromCategoricalToNumericalOrdinal(
          column_data = df.original[[i]], 
          column_levels = column.levels[[var.aux1]][-1], 
          column_name = colnames(df.original)[i]
        )
      )
      var.aux1 <- var.aux1 + 1
    }
    else if (convertion.types[[i]][2] == 2)
    {
      df.aux <- cbind(
        df.aux,
        convertColumnFromCategoricalToNumericalThroughBinarization(
        df.original = df.original,
        column = i,
        columnName = colnames(df.original)[i],
        columnLevels = c(column.levels[[var.aux1]][-1])
        )
      )
      var.aux1 <- var.aux1 + 1
    }
    else if (convertion.types[[i]][2] == 3)
    {
      current_col <- data.frame(matrix(
        data = df.original[, i],
        nrow = length(df.original[, i]),
        ncol = 1
      ))
      colnames(current_col) <- colnames(df.original)[i]
      
      df.aux <- cbind(df.aux, current_col)
    }
    # var.aux1 <- var.aux1 + 1
  }
  
  df.aux <- cbind(df.aux, df.original[ncol(df.original)])
  df.aux <- df.aux[, -1]
  
  return(df.aux)
}

#' @description Hide a column in a converted dataframe.
#' 
#' @param df_converted The converted dataframe.
#' @param columns_to_hide The indexes for the columns that must be hidden.
#' @param convrt_typs Represents the types of convertion that the dataframe
#' suffered.
#' @param convrt_lvls Represents the possible values for the original data in
#' the original categorical columns.
#' @param original_colnames The original names for the columns.
#' 
#' @return A dataframe containing the converted data, minus the columns which
#' were hidden.
hideColumnsOfConvertedDataframe <- function(df_converted, columns_to_hide,
  convrt_typs, convrt_lvls, original_colnames)
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
    }
  }
  
  indexes_to_hide <- c()
  for(i in colnames_to_hide)
  {
    indexes_to_hide <- c(indexes_to_hide, which(colnames(df_converted) == i))
  }
  
  return(df_converted[, -c(indexes_to_hide)])
}

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
  
  if(length(aList) >= 1)
  {
    for(i in 1:length(aList))
    {
      cElement <- aList[[i]]
      if(length(which(elementsToHide == cElement[1])) == 0)
      {
        auxList[[auxVar]] <- cElement
        auxVar <- auxVar + 1
      }
    }
  }
  
  return(auxList)
}
