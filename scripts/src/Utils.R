
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
}

#' @description Order the instances in a dataframe by rownames.
#' @param df_to_order A dataframe to be ordered.
#' @return The dataframe, with it's instances ordered.
orderDataframeByRowname <- function(df_to_order)
{
  return(df_to_order[order(as.numeric(rownames(df_to_order))), ])
}

#' Load a .csv file from de HD.
#' @param df.location a path to a .csv file.
#' @param df.name the .csv filename.
#' @param df.sufix the sufix for the .csv file.
#' @return a dataframe with the data from the .csv file.
readFromCsv <- function(df.location, df.name, df.sufix = "")
{
  df.aux <- read.csv(paste(df.location,
                           df.name, 
                           df.sufix, 
                           ".csv", sep = ""), 
                     stringsAsFactors = TRUE)
  return(df.aux)
}

#' Write a dataframe into a .csv file.
#' @param df.toWrite a dataframe that is supposed to be written
#' into a .csv file.
#' @param df.location the path in which the .csv file must be written.
#' @param df.name the name of the .csv file.
#' @param df.sufix the sufix of the .csv file.
writeToCsv <- function(df.toWrite, df.location, df.name, df.sufix = "")
{
  write.csv(df.toWrite, paste(df.location, 
                              df.name,
                              df.sufix,
                              ".csv", sep = ""), 
            quote = FALSE, 
            na = "?", row.names = FALSE)
}
