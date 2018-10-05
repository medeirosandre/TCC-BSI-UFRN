
#' Autistic Screening Adult:
#' 
#' nome do arquivo: autism-adult.csv
#' numero de classes: 2
#' numero de atributos: 21
#' numero de instancias: 704
#' numero de instancias completas: 609
#' numero de valores faltosos: 192
#' % instancias completas: 86
#' 
#' url: https://archive.ics.uci.edu/ml/datasets/Autism+Screening+Adult

df_name <- df_names[4]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertSpecificColumnsFromCatToNum(
  df_to_convert = df_original,
  columns_to_convert = c(seq(1, 11), 18)
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:10)
{
  convert_types[[i]] <- c(i, 1)
}
convert_types[[11]] <- c(11, 3)
convert_types[[12]] <- c(12, 1)
convert_types[[13]] <- c(13, 2)
convert_types[[14]] <- c(14, 1)
convert_types[[15]] <- c(15, 1)
convert_types[[16]] <- c(16, 2)
convert_types[[17]] <- c(17, 1)
convert_types[[18]] <- c(18, 3)
convert_types[[19]] <- c(19, 1)
convert_types[[20]] <- c(20, 2)

convert_lvls <- list()
for(i in 1:10)
{
  convert_lvls[[i]] <- c(i, c(0, 1))
}
convert_lvls[[11]] <- c(12, c(levels(df_original[[12]])))
convert_lvls[[12]] <- c(13, c(levels(df_original[[13]])))
convert_lvls[[13]] <- c(14, c(levels(df_original[[14]])))
convert_lvls[[14]] <- c(15, c(levels(df_original[[15]])))
convert_lvls[[15]] <- c(16, c(levels(df_original[[16]])))
convert_lvls[[16]] <- c(17, c(levels(df_original[[17]])))
convert_lvls[[17]] <- c(19, c(levels(df_original[[19]])))
convert_lvls[[18]] <- c(20, c(levels(df_original[[20]])))

# 1 = mean
# 2 = fashion

fill_na_using <- c(rep(2, 10), 1, rep(2, 6), 1, rep(2, 2))
