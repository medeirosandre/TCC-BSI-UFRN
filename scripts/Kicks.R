
#' Kicks:
#' 
#' nome do arquivo: kick.csv
#' numero de classes: 2
#' numero de atributos: 33
#' numero de instancias: 72983
#' numero de instancias completas: 3274
#' numero de valores faltosos: 149271
#' % instancias completas: .04
#' 
#' url: https://www.openml.org/d/41162

df_name <- df_names[12]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- pushClassToTheEnd(df_original, 1, "class")
df_original <- convertSpecificColumnsFromCatToNum(
  df_to_convert = df_original,
  columns_to_convert = c(
    1, 3, 4, 11, 13, 17, 18, 19, 20, 21, 22, 23, 24, 27, 28, 30, 31, 32
  )
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(
  c(1, 3), c(2, 2), c(3, 1), c(4, 1), c(5, 2), c(6, 2), c(7, 2), c(8, 2),
  c(9, 2), c(10, 1), c(11, 1), c(12, 2), c(13, 3), c(14, 2), c(15, 1),
  c(16, 2), c(17, 3), c(18, 3), c(19, 3), c(20, 3), c(21, 3), c(22, 3),
  c(23, 3), c(24, 3), c(25, 1), c(26, 1), c(27, 1), c(28, 1), c(29, 2),
  c(30, 3), c(31, 1), c(32, 3)
)

convert_lvls <- list(
  c(2, c(as.character(unique(df_original$Auction)))),
  c(3, c(2001:2010)),
  c(4, c(0:9)),
  c(5, c(as.character(unique(df_original$Make)))),
  c(6, c(as.character(unique(df_original$Model)))),
  c(7, c(as.character(unique(df_original$Trim)))[-which(is.na(c(as.character(unique(df_original$Trim)))))]),
  c(8, c(as.character(unique(df_original$SubModel)))[-which(is.na(c(as.character(unique(df_original$SubModel)))))]),
  c(9, c(as.character(unique(df_original$Color)))[-which(is.na(c(as.character(unique(df_original$Color)))))]),
  c(10, c("AUTO", "MANUAL")),
  c(11, c(0:3)),
  c(12, c("Alloy", "Covers", "Special")),
  c(14, c("AMERICAN", "OTHER", "OTHER ASIAN", "TOP LINE ASIAN")),
  c(15, c(
    "SPORTS", "COMPACT", "MEDIUM", "SMALL SUV", "CROSSOVER", "MEDIUM SUV",
    "SMALL TRUCK", "LARGE", "SPECIALTY", "VAN", "LARGE TRUCK", "LARGE SUV")
  ),
  c(16, c("CHRYSLER", "FORD", "GM", "OTHER")),
  c(25, c("NO", "YES")),
  c(26, c("GREEN", "RED")),
  c(27, c(levels(as.factor(df_original$BYRNO)))),
  c(28, c(levels(as.factor(df_original$VNZIP1)))),
  c(29, c(levels(as.factor(df_original$VNST)))),
  c(31, c("0", "1"))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c(1, rep(2, 11), 1, rep(2, 3), rep(1, 8), rep(2, 5), 1, 2, 1)

source("tcc/scripts/src/ExecuteTechniques.R")
