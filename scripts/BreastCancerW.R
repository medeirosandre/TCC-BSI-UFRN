
#' Breast Cancer Wisconsin:
#' 
#' nome do arquivo: breast-w.csv
#' numero de classes: 2
#' numero de atributos: 10
#' numero de instancias: 699
#' numero de instancias completas: 683
#' numero de valores faltosos: 16
#' % instancias completas: 97
#' 
#' url: https://www.openml.org/d/15

df_name <- df_names[7]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:9)
{
  convert_types[[i]] <- c(i, 3)
}

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c(rep(2, 9))
