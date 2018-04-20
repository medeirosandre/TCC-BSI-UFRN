
##############################
###        Functions       ###
##############################


##############################
##      Data Convertion     ##
##############################

# Categorical to Numerical data
convertColCatToNum <- function(dataFrame, column)
{
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
        auxDataFrame[j,k] <- as.numeric(1)
      }
      else
      {
        auxDataFrame[j,k] <- as.numeric(0)
      }
    }
  }
  
  for (l in 1:length(lvls))
  {
    lvls[l] = paste(column, "_", lvls[l])
  }
  colnames(auxDataFrame) <- lvls
  
  return(auxDataFrame)
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
##      Data Imputation     ##
##############################

fillNAs <- function(dataframe.original, dataframe.clean, dataframe.treated, number.k)
{
  levels <- dataframe.clean[[47]]
  dataframe.train <- dataframe.clean[,-47]
  
  k <- knn(dataframe.train, dataframe.train, levels, k=number.k, algorithm = "cover_tree")
  indices <- attr(k, "nn.index")
  
  for(i in 1:nrow(dataframe.original))
  {
    for(j in 1:(ncol(dataframe.original)-1))
    {
      if(is.na(dataFrame.original[i,j]) || dataframe.original[i,j] == "")
      {
        print(i)
        print(j)
        nearest.neighbors <- dataframe.treated[indices[i,-1],]
        if(is.numeric(dataFrame.original[[j]]))
        {
          dataframe.original[i,j] <- mean(nearest.neighbors[,j])
        }
        else{
          dataframe.original[i,j] <- moda(nearest.neighbors[,j])
        }
      }
    }
  }
  
  return(dataframe.original)
}


################
##    Moda    ##
################

# TODO tratar empates e sorteio de NAs
moda <- function(column) 
{
  lvls <- as.factor(as.vector(nearest.neighbors[[6]]))
} 
