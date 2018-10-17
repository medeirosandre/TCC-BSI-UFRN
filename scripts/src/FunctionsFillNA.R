
#' Fill missing values in a single row, based on a dataframe.
#' 
#' @param df.toAnalize dataframe from which the value to be used is obtained.
#' @param row a single row of a dataframe in which the missing values must be
#' filled.
#' @param row.naElements a vector containing the indexes for the columns that
#' contain missing values.
#' @param fillUsing represents the method to be used to input the missing value
#' (1 = mean, 2 = fashion).
#' 
#' @return a dataframe containing a sigle row with it's missing values filled.
fillNAInARow <- function(df.toAnalize, row, row.naElements, fillUsing)
{
  for (j in 1:length(row.naElements)) {
    if (fillUsing[row.naElements[j]] == 1)
    {
      # value.toReplace <- findMean(df.toAnalize, row.naElements[j])
      value.toReplace <- findMean(df.toAnalize[[row.naElements[j]]])
    }
    else
    {
      # value.toReplace <- findFashion(df.toAnalize, row.naElements[j])
      value.toReplace <- findFashion(df.toAnalize[[row.naElements[j]]])
    }
    row[, row.naElements[j]] <- value.toReplace
  }
  
  return(row)
}

#' @description Fill <NA> from original dataframe based in the original
#' complete cases of said dataframe, plus, if necessary, the rows with <NA>
#' recently filled.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original
#' dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an
#' original dataframe.
#' @param fill_na_using a vector containing the indicators to which technique
#' is to be used to fill the NA in a row, mean or fashion.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
fillNAWithCompleteDataset <- function(df.noNA, df.onlyNA, fill_na_using,
  appending = T)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    if(appending)
    {
      current.row <- fillNAInARow(
        df.toAnalize = df.aux, 
        row = current.row, 
        row.naElements = column.na, 
        fillUsing = fill_na_using
      )
    }
    else
    {
      current.row <- fillNAInARow(
        df.toAnalize = df.noNA, 
        row = current.row, 
        row.naElements = column.na, 
        fillUsing = fill_na_using
      )
    }
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  df.aux <- orderDataframeByRowname(df.aux)
  
  return(df.aux)
}

#' @description Fill <NA> from the original dataframe based in the original
#' complete cases of said datafame, plus, the rows with <NA> recently filled,
#' having the same classes as the case which must be filled.
#' 
#' @param df.noNA a dataframe containing only complete cases of an original
#' dataframe.
#' @param df.onlyNA a dataframe containing only the incomplete cases of an
#' original dataframe.
#' @param fill_na_using a vector containing the indicators to which technique
#' is to be used to fill the NA in a row, mean or fashion.
#' 
#' @return a dataframe representing the original data with it's <NA> filled.
fillNAWithDatasetOfCasesClass <- function(df.noNA, df.onlyNA, fill_na_using,
  appending = T)
{
  df.aux <- df.noNA
  for (i in 1:nrow(df.onlyNA)) 
  {
    current.row <- df.onlyNA[i, ]
    column.na <- findWhichElementsInRowAreNA(current.row)
    
    if(appending)
    {
      df.aux2 <- getCasesOfSpecificClass(
        df.original = df.aux, 
        className = as.character(current.row[, ncol(current.row)])
      )
    }
    else
    {
      df.aux2 <- getCasesOfSpecificClass(
        df.original = df.noNA,
        className = as.character(current.row[, ncol(current.row)])
      )
    }
    
    current.row <- fillNAInARow(df.aux2, current.row, column.na, fill_na_using)
    
    df.aux <- appendRowIntoDataframe(df.aux, current.row)
  }
  
  df.aux <- orderDataframeByRowname(df.aux)
  
  return(df.aux)
}