
#' Polish Companies Bankrupcy
#' 
#' nome do arquivo: 3year.csv
#' numero de classes: 2
#' numero de atributos: 65
#' numero de instancias: 10503
#' numero de instancias completas: 4885
#' numero de valores faltosos: 9888
#' % instancias complestas: 46
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/Polish+companies+bankruptcy+data

df_original <- readFromCsv(
  df.location = df_locations[1],
  df.name = df_name
)
df_original <- dropLevelFromDataframe(
  df.original = df_original,
  levelToDrop = "?"
)
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

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
