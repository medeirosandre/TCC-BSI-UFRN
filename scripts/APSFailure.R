
#' APS Failure at Scania Trucks:
#' 
#' nome do arquivo: aps-failure.csv
#' numero de classes 2
#' numero de atributos: 171
#' numero de instancias: 60000
#' numero de instancias completas: 591
#' numero de valores faltosos: 850015
#' % instancias completas: .009
#' 
#' url: http://archive.ics.uci.edu/ml/datasets/APS+Failure+at+Scania+Trucks

df_name <- df_names[11]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- pushClassToTheEnd(df_original, 1, "class")
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:170)
{
  convert_types[[i]] <- c(i, 3)
}

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c(rep(1, 170))

source("scripts/src/ExecuteTechniques.R")
