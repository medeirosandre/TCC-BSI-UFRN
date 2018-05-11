
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

# d.o = dataframe.original
# d.c = dataframe.clean
# d.t = dataframe.treated
fillNAs <- function(d.o, d.c, d.t, num.k)
{
  lvls <- d.c[[length(d.c)]]
  d.train <- d.c[,-length(d.c)]
  
  k <- knn(d.train, d.train, lvls, k=num.k, algorithm = "cover_tree")
  indices <- attr(k, "nn.index")
  
  for(i in 1:nrow(d.o))
  {
    for(j in 1:(ncol(d.o)-1))
    {
      if(is.na(d.o[i,j]) || d.o[i,j] == "")
      {
        nearest.neighbors <- d.t[indices[i,-1],]
        if(is.numeric(d.o[[j]]))
        {
          d.o[i,j] <- media(nearest.neighbors ,j)
        }
        else{
          d.o[i,j] <- moda(nearest.neighbors, j)
        }
      }
    }
  }
  
  return(d.o)
}


########################
##    Media e Moda    ##
########################

# y = nearest.neighbors
# c = column
media <- function(y, c)
{
  x <- y[complete.cases(y[,c]),c]
  
  if(length(x) == 0)
  {
    return(0)
  }
  
  return(mean(x))
}

# y = nearest.neighbors
# c = column
moda <- function(y, c) 
{
  x <- as.factor(
    as.vector(
      y[which(as.vector(y[[c]]) != "ZZZ"),c]))
  
  l <- summary(x)
  m <- names(sort(l, decreasing = T))
  
  return(m[1])
} 
