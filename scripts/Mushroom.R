
#' Mushroom
#' 
#' nome do arquivo: mushroom.csv
#' numero de classes: 2
#' numero de atributos: 23
#' numero de instancias: 8124
#' numero de instancias completas: 5644
#' numero de valores faltosos: 2480
#' % instancias completas: 69
#' 
#' url: https://www.openml.org/d/24

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- pushClassToTheEnd(df_original)

columns_to_normalize <- c()

convert_types <- list(c(1, 2), c(2, 2), c(3, 2), c(4, 1), c(5, 2), 
                      c(6, 2), c(7, 1), c(8, 2), c(9, 2), c(10, 2), 
                      c(11, 2), c(12, 2), c(13, 2), c(14, 2), c(15, 2), 
                      c(16, 2), c(17, 2), c(18, 1), c(19, 2), c(20, 2), 
                      c(21, 1), c(22, 2))
convert_lvls <- list(c(1, c(levels(df_original[[1]]))),
                     c(2, c(levels(df_original[[2]]))),
                     c(3, c(levels(df_original[[3]]))),
                     c(4, "f", "t"),
                     c(5, c(levels(df_original[[5]]))),
                     c(6, c(levels(df_original[[6]]))),
                     c(7, "d", "c", "w"),
                     c(8, c(levels(df_original[[8]]))),
                     c(9, c(levels(df_original[[9]]))),
                     c(10, c(levels(df_original[[10]]))),
                     c(11, c(levels(df_original[[11]]))),
                     c(12, c(levels(df_original[[12]]))),
                     c(13, c(levels(df_original[[13]]))),
                     c(14, c(levels(df_original[[14]]))),
                     c(15, c(levels(df_original[[15]]))),
                     c(16, c(levels(df_original[[16]]))),
                     c(17, c(levels(df_original[[17]]))),
                     c(18, "n", "o", "t"),
                     c(19, c(levels(df_original[[19]]))),
                     c(20, c(levels(df_original[[20]]))),
                     c(21, "y", "s", "c", "v", "n", "a"),
                     c(21, c(levels(df_original[[21]]))))

fill_na_using <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
