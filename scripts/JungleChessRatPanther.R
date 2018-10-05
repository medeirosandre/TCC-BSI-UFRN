
#' Jungle Chess 2pcs Endgame Rat Panther:
#' 
#' nome do arquivo: jngl-chs-2pcs-ndgm-rat-pntr.csv
#' numero de classes: 2
#' numero de atributos: 47
#' numero de instancias: 5880
#' numero de instancias completas: 4704
#' numero de valores faltosos: 3528
#' % instancias completas: 80
#' 
#' url: https://www.openml.org/d/41002

df_name <- df_names[11]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertSpecificColumnsFromCatToNum(
  df_to_convert = df_original,
  columns_to_convert = c(seq(1, 3), seq(5, 21), seq(23, 37), 40, seq(42, 46))
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list()
for(i in 1:10)
{
  convert_types[[i]] <- c(i, 3)
}
for(i in 11:18)
{
  convert_types[[i]] <- c(i, 1)
}
for(i in 19:27)
{
  convert_types[[i]] <- c(i, 3)
}
for(i in 28:46)
{
  convert_types[[i]] <- c(i, 1)
}
convert_types[[4]] <- c(4, 2)
convert_types[[8]] <- c(8, 1)
convert_types[[16]] <- c(16, 3)
convert_types[[22]] <- c(22, 2)
convert_types[[35]] <- c(35, 3)
convert_types[[38]] <- c(38, 2)
convert_types[[40]] <- c(40, 3)
convert_types[[42]] <- c(42, 3)

convert_lvls <- list(
  c(4, c(levels(df_original[[4]]))),
  c(8, c(0, 1)),
  c(11, c(0, 1)),
  c(12, c(0, 1)),
  c(13, c(0, 1)),
  c(14, c(0, 1)),
  c(15, c(0, 1)),
  c(17, c(0, 1)),
  c(18, c(0, 1)),
  c(22, c(levels(df_original[[22]]))),
  c(28, c(0, 1)),
  c(29, c(0, 1)),
  c(30, c(0, 1)),
  c(31, c(0, 1)),
  c(32, c(0, 1)),
  c(33, c(0, 1)),
  c(34, c(0, 1)),
  c(36, c(0, 1)),
  c(37, c(0, 1)),
  c(38, c("w", "b", "d")),
  c(39, c(levels(df_original[[39]]))),
  c(41, c(levels(df_original[[41]]))),
  c(43, c(0, 1)),
  c(44, c(0, 1)),
  c(45, c(0, 1)),
  c(46, c(0, 1))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c(
  rep(1, 3), 2, rep(1, 3), 2, rep(1, 2), rep(2, 5), 1, rep(2, 2), rep(1, 3), 2,
  rep(1, 5), rep(2, 7), 1, rep(2, 4), 1, 2, 1, rep(2, 4)
)
