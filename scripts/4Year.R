
#' Polish Companies Bankrupcy
#' 
#' nome do arquivo: 4Year.csv
#' numero de classes: 2
#' numero de atributos: 65
#' numero de instancias: 9792
#' numero de instancias completas: 4769
#' numero de valores faltosos: 8776
#' % instancias complestas: 49
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/Polish+companies+bankruptcy+data

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- convertCategoricalToNumerical(df_original)

columns_to_normalize <- c()
# columns_to_normalize <- c(1:64)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:64)
{
  convert_types[[i]] <- c(i, 3)
}

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c(rep(1, 64))
