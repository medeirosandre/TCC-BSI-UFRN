
#' Hip Measurements:
#' 
#' nome do arquivo: hip.csv
#' numero de classes: 2
#' numero de atributos: 8
#' numero de instancias: 54
#' numero de instancias completas: 24
#' numero de valores faltosos: 120
#' % instancias completas: 44
#' 
#' url: https://www.openml.org/d/898

df_name <- df_names[1]
# df_name <- df_names[3]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

convert_types <- list(c(1, 3), c(2, 3), c(3, 3), c(4, 3),
                      c(5, 3), c(6, 3), c(7, 3))

convert_lvls <- list()

fill_na_using <- c(1,1,1,1,1,1,1)

source("tcc/scripts/src/ExecuteTechniques.R")
