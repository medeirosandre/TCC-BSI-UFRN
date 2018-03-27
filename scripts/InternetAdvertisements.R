##############################################
###   Cleaning Internet Advertisements     ###
##############################################


# import libraries
library(readr)  # reading
library(DMwR)   # data cleaning

# import data frame from .csv file
InternetAdvertisements <- read.csv("tcc/databases/Internet Advertisements.csv")

# create final data frame
Clean_Internet_Advertisements <- data.frame()
Clean_Internet_Advertisements <- data.frame(InternetAdvertisements)

# switching ? to NAs
Clean_Internet_Advertisements <- droplevels(Clean_Internet_Advertisements, exclude = "?")

# converting original numerical data
for(i in names(Clean_Internet_Advertisements)) {
  if(!is.numeric(Clean_Internet_Advertisements[[i]])) {
    Clean_Internet_Advertisements[[i]] <- as.double(Clean_Internet_Advertisements[[i]])
  }
}

# maintaining original class
Clean_Internet_Advertisements$class <- InternetAdvertisements$class

# save data frame to .csv file
write.csv(Clean_Internet_Advertisements, "tcc/clean_databases/Clean_Internet_Advertisements.csv", na = "", row.names = FALSE)

# obtain data frame from .csv file (now without written NAs)
Clean_Internet_Advertisements <- read.csv("tcc/clean_databases/Clean_Internet_Advertisements.csv")

# using knn to fill empty tuples
Clean_Internet_Advertisements <- knnImputation(Clean_Internet_Advertisements, k = 10)

# save dataframe to .csv file
write.csv( Clean_Internet_Advertisements, "tcc/clean_databases/Clean_Internet_Advertisements.csv", na = "", row.names = FALSE)


# feature selection

pca_internetAd <- prcomp(Clean_Internet_Advertisements[,-1559])
eig_val <- get_eigenvalue(pca_internetAd)

class <- Clean_Internet_Advertisements$class

write.csv(cbind(Clean_Internet_Advertisements[,1:3], class), "tcc/clean_databases/Clean_Internet_Advertisements.csv", row.names = FALSE)