
#' Mammographic Mass:
#' 
#' nome do arquivo: mammographic-mass.csv
#' numero de classes: 2
#' numero de atributos: 6
#' numero de instancias: 961
#' numero de instancias completas: 830
#' numero de valores faltosos: 160
#' % instancias completas: 83
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/Mammographic+Mass

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- convertCategoricalToNumerical(df_original)

columns_to_normalize <- c(1:5)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(c(1, 3), c(2, 3), c(3, 3), c(4, 3), c(5, 3))

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c(2, 2, 2, 2, 2)
