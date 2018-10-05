
#' TODO esta base possui classes com poucas instâncias, não o suficiente para
#' rodar knn com instancias da mesma classe.

#' Primary Tumor:
#' 
#' nome do arquivo: primary-tumor.csv
#' numero de classes: 21
#' numero de atributos: 18
#' numero de instancias: 339
#' numero de instancias completas: 132
#' numero de valores faltosos: 225
#' % instancias completas: 38
#' 
#' url: https://www.openml.org/d/171

df_name <- df_names[8]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(
  c(1, 1), c(2, 1), c(3, 2), c(4, 1), c(5, 1), c(6, 1), c(7, 1), c(8, 1),
  c(9, 1), c(10, 1), c(11, 1), c(12, 1), c(13, 1), c(14, 1), c(15, 1),
  c(16, 1), c(17, 1)
)

convert_lvls <- list(
  c(1, c("<30", "30-59", ">=60")), c(2, c("female", "male")),
  c(3, c("adeno", "anaplastic", "epidermoid")),
  c(4, c("poorly", "fairly", "well")),
  c(5, c("no", "yes")), c(6, c("no", "yes")), c(7, c("no", "yes")),
  c(8, c("no", "yes")), c(9, c("no", "yes")), c(10, c("no", "yes")),
  c(11, c("no", "yes")), c(12, c("no", "yes")), c(13, c("no", "yes")),
  c(14, c("no", "yes")), c(15, c("no", "yes")), c(16, c("no", "yes")),
  c(17, c("no", "yes"))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c()
for(i in 1:17)
{
  fill_na_using <- c(fill_na_using, 2)
}
