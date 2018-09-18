
#' Imports packages and sets dataframes locations and names

# install.packages("FNN")
# install.packages("factoextra")

#' Implements K-nn
library("FNN")
#' Rename levels
library("plyr")
#' Read files
library("readr")
library("farff")

df_locations <- c("tcc/originalDatasets/", "tcc/cleanDatasets/")
df_names <- c("vote")
techniques_sufix <- c("_MediaEModaConjuntoCompletoAppending", "_MediaEModaConjuntoCompletoNotAppending",
                      "_MediaEModaConjuntoDeMesmaClasseAppending", "_MediaEModaConjuntoDeMesmaClasseNotAppending",
                      "_MediaEModa3NNConjuntoCompletoAppending", "_MediaEModa5NNConjuntoCompletoAppending",
                      "_MediaEModa7NNConjuntoCompletoAppending", "_MediaEModa9NNConjuntoCompletoAppending")
