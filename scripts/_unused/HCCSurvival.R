
#' TODO esta base possui apenas 8 exemplos totalmente preenchidos, o que levanda
#' warnings nos métodos do knn.

#' HCC Survival:
#' 
#' nome do arquivo: hcc-survival.csv
#' numero de classes: 2
#' numero de atributos: 50
#' numero de instancias: 165
#' numero de instancias completas: 8
#' numero de valores faltosos: 826
#' % instancias completas: .04
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/HCC+Survival

df_name <- df_names[10]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertCategoricalToNumerical(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:49)
{
  convert_types[[i]] <- c(i, 3)
}

convert_lvls <- list()

# 1 = mean
# 2 = fashion

fill_na_using <- c()
for(i in 1:23)
{
  fill_na_using <- c(fill_na_using, 2)
}
fill_na_using <- c(fill_na_using, c(1, 1, 1, 2, 2, 2))
for(i in 1:20)
{
  fill_na_using <- c(fill_na_using, 1)
}
