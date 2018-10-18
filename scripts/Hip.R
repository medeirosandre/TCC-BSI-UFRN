
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

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

columns_to_normalize <- c(1:7)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(c(1, 3), c(2, 3), c(3, 3), c(4, 3),
                      c(5, 3), c(6, 3), c(7, 3))

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c(1,1,1,1,1,1,1)
