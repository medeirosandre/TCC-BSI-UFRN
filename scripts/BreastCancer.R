
#' Breast Cancer:
#' 
#' nome do arquivo: breast-cancer.csv
#' numero de  classes: 2
#' numero de  atributos 16
#' numero de  instancias 286
#' numero de  instancias completas: 277
#' numero de  valores faltosos: 9
#' % instancias completas: 96
#' 
#' url: https://www.openml.org/d/13

df_name <- df_names[6]

df_original <- readFromCsv(
  df.location = df_locations[1],
  df.name = df_name
)
df_original <- pushClassToTheEnd(
  df.original = df_original,
  column = 1,
  columnName = "class"
)
df_original <- dropLevelFromDataframe(
  df.original = df_original,
  levelToDrop = "?"
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(
  c(1, 1), c(2, 2), c(3, 1), c(4, 1), c(5, 1), c(6, 3), c(7, 2), c(8, 2),
  c(9, 1)
)

convert_lvls <- list(
  c(1, c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79")),
  c(2, c("ge40", "lt40", "premeno")),
  c(3, c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54"
  )),
  c(4, c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "24-26")),
  c(5, c("no", "yes")),
  c(7, c("left", "right")),
  c(8, c("central", "left_low", "left_up", "right_low", "right_up")),
  c(9, c("no", "yes"))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c(2, 2, 2, 2, 2, 2, 2, 2, 2)

source("tcc/scripts/src/ExecuteTechniques.R")
