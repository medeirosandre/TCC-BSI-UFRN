
#' Congrational Voting Records
#' 
#' nome do arquivo: vote.csv
#' numero de classes: 2
#' numero de atributos: 17
#' numero de instancias: 435
#' numero de instancias completas: 232
#' numero de valores faltosos: 392
#' % instancias completas: 53
#' 
#' url: https://www.openml.org/d/56

# run on windows
# setwd("D:/workspace/UFRN/R")
# run on linux
setwd("~/Workspace/UFRN")

source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")

df_name <- df_names[1]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- dropInstancesWithFullNA(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

convert_types <- list(c(1, 1), c(2, 1), c(3, 1), c(4, 1), c(5, 1),
                      c(6, 1), c(7, 1), c(8, 1), c(9, 1), c(10, 1),
                      c(11, 1), c(12, 1), c(13, 1), c(14, 1), c(15, 1),
                      c(16, 1))

convert_lvls <- list(c(1, "n", "y"), c(2, "n", "y"), c(3, "n", "y"),
                     c(4, "n", "y"), c(5, "n", "y"), c(6, "n", "y"),
                     c(7, "n", "y"), c(8, "n", "y"), c(9, "n", "y"),
                     c(10, "n", "y"), c(11, "n", "y"), c(12, "n", "y"),
                     c(13, "n", "y"), c(14, "n", "y"), c(15, "n", "y"),
                     c(16, "n", "y"))

fill_na_using <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

# df_final <- fillNAWithCompleteDatasetAppending(df_noNA, df_onlyNA, fill_na_using)
# writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[1])
# 
# df_final <- fillNAWithCompleteDatasetNotAppending(df_noNA, df_onlyNA, fill_na_using)
# writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[2])
# 
# df_final <- fillNAWithDatasetOfCasesClassAppending(df_noNA, df_onlyNA, fill_na_using)
# writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[3])
# 
# df_final <- fillNAWithDatasetOfCasesClassNotAppending(df_noNA, df_onlyNA, fill_na_using)
# writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[4])
# 
# df_final <- fillNAWithKNNFromCompleteDatasetAppending(df_noNA, df_onlyNA, convert_types, convert_lvls, fill_na_using, 3)
# writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[5])

k <- c()
for(j in 1:16){
  for(i in 1:nrow(df_original))
  {
    if(length(which(is.na(df_original[i, ]))) == j)
    {
      k <- c(k, j)
      print(i)
    }
  }
}
