
#' @description List the files in a folder.
#' @param path_to_look path to the folder.
#' @param pattern pattern to look for in the filenames.
#' @return a vector of the found filenames.
getFileNames <- function(path_to_look, pattern = NULL)
{
  return(list.files(
    path = path_to_look,
    pattern = pattern
  ))
}

#' @description Get the starting number for KNN based methods.
#' @param path_to_look path to search for files.
#' @param base_name name of the base.
#' @param pattern pattern in the filename.
#' @return the starting number for K.
getStartOfKVector <- function(path_to_look, base_name = NULL, pattern = NULL)
{
  return(length(
    getFileNames(
      path_to_look = path_to_look,
      pattern = paste("^.*", base_name, ".*.*", pattern, ".*.$", sep = "")
    )
  ) + 1)
}

#' @description Get which techniques must be applied to the dataset.
#' @param techniques_completed The techniques that were applied.
#' @return A vector indicating the techniques to be applied.
getWhichTechniquesToAplly <- function(techniques_completed)
{
  techniques_to_apply <- c()
  if(length(techniques_completed) == 20)
  {
    return(techniques_to_apply)
  }
  
  if(length(techniques_completed) < 1)
  {
    techniques_to_apply <- c(techniques_to_apply, 1)
  }
  
  if(length(techniques_completed) < 2)
  {
    techniques_to_apply <- c(techniques_to_apply, 2)
  }
  
  if(length(techniques_completed) < 3)
  {
    techniques_to_apply <- c(techniques_to_apply, 3)
  }
  
  if(length(techniques_completed) < 4)
  {
    techniques_to_apply <- c(techniques_to_apply, 4)
  }
  
  if(length(techniques_completed) < 8)
  {
    techniques_to_apply <- c(techniques_to_apply, 5)
  }
  
  if(length(techniques_completed) < 12)
  {
    techniques_to_apply <- c(techniques_to_apply, 6)
  }
  
  if(length(techniques_completed) < 16)
  {
    techniques_to_apply <- c(techniques_to_apply, 7)
  }
  
  if(length(techniques_completed) < 20)
  {
    techniques_to_apply <- c(techniques_to_apply, 8)
  }
  
  return(techniques_to_apply)
}

#' @description Install the needed packages if they are not installed,
#' then load the packages.
#' @author Arthur Gorgonio
installNeededPackages <- function() {
  packages <- c("FNN", "plyr", "readr", "RWeka")
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE)
  }
  
  rm(packages)
}

#' @description Order the instances in a dataframe by rownames.
#' @param df_to_order A dataframe to be ordered.
#' @return The dataframe, with it's instances ordered.
orderDataframeByRowname <- function(df_to_order)
{
  return(df_to_order[order(as.numeric(rownames(df_to_order))), ])
}

#' @description Load a .csv file from de HD.
#' @param df.location a path to a .csv file.
#' @param df.name the .csv filename.
#' @param df.sufix the sufix for the .csv file.
#' @return a dataframe with the data from the .csv file.
readFromCsv <- function(df.location, df.name, df.sufix = "")
{
  return(read.csv(
    paste(df.location, df.name, df.sufix, ".csv", sep = ""),
    stringsAsFactors = T
  ))
}

#' @description Write a dataframe into a .csv file.
#' @param df.toWrite a dataframe that is supposed to be written
#' into a .csv file.
#' @param df.location the path in which the .csv file must be written.
#' @param df.name the name of the .csv file.
#' @param df.sufix the sufix of the .csv file.
writeToCsv <- function(df.toWrite, df.location, df.name, df.sufix = "")
{
  write.csv(
    x = df.toWrite,
    file = paste(df.location, df.name, df.sufix, ".csv", sep = ""), 
    quote = F, 
    na = "?",
    row.names = F
  )
}
