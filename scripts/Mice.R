
#' Mice Protein Expression:
#' 
#' nome do arquivo: mice.csv
#' numero de classes: 8
#' numero de atributos: 82
#' numero de instancias: 1080
#' numero de instancias completas: 552
#' numero de valores faltosos: 1114
#' % instancias completas: 51
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/Mice+Protein+Expression
#' url: https://www.openml.org/d/40966

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- convertCategoricalToNumerical(df_original)

columns_to_normalize <- c(1:77)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:77)
{
  convert_types[[i]] <- c(i, 3)
}
for(i in 78:80)
{
  convert_types[[i]] <- c(i, 2)
}

convert_lvls <- list(
  c(78, c(1, 2)), c(79, c(1, 2)), c(80, c(1, 2))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c()
for(i in 1:77)
{
  fill_na_using <- c(fill_na_using, 1)
}
for(i in 1:3)
{
  fill_na_using <- c(fill_na_using, 2)
}
