##############################################
###              Functions                 ###
##############################################


##############################
##    Data Convertion
##############################

# Categorical to Numerical data
convertColCatToNum <- function(dataFrame, column)
{
  newDataFrame <- data.frame(matrix(1, 
                                    nrow = nrow(dataFrame), 
                                    ncol = 1))
  
  lvls <- levels(dataFrame[[column]])
  auxDataFrame <- data.frame(matrix(lvls, 
                                    nrow = nrow(dataFrame), 
                                    ncol = length(lvls)), 
                             stringsAsFactors = FALSE)
  colnames(auxDataFrame) <- lvls
  
  for (j in 1:nrow(dataFrame))
  {
    for (k in lvls)
    {
      aux <- dataFrame[j, column]
      if (aux == k)
      {
        auxDataFrame[j,k] = 1
      }
      else
      {
        auxDataFrame[j,k] = 0
      }
    }
  }
  
  for (l in 1:length(lvls))
  {
    lvls[l] = paste(column, "_", lvls[l])
  }
  colnames(auxDataFrame) <- lvls
  
  newDataFrame <- cbind(newDataFrame[,-1], auxDataFrame)
  
  return(newDataFrame)
}

# Categorical to Numerical data (Ordinal)
convertColCatToNumOrd <- function(dataFrame, column, levels)
{
  newDataFrame <- data.frame(matrix(1, 
                                    nrow = nrow(dataFrame), 
                                    ncol = 1))
  colnames(newDataFrame) <- colnames(dataFrame)[column]
  
  for (i in 1:nrow(dataFrame))
  {
    for (j in 1:length(levels))
    {
      if (dataFrame[i, column] == levels[j])
      {
        newDataFrame[i, 1] <- j
      }
    }
  }

  return(newDataFrame)
}

##############################
##    Data Inputation
##############################

# Filling NAs
fillNAs <- function(dataFrame, method, neighbors)
{
  if(method == 1)
  {
    dataFrame <- centralImputation(dataFrame)
  }
  else if(method == 2)
  {
    dataFrame <- knnImputation(dataFrame, k = neighbors)
  }
  
  return(dataFrame)
}
