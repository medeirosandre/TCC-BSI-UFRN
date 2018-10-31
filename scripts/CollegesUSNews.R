
#' 
#' 
#' nome do arquivo: CollegesUSNews.csv
#' numero de classes: 2
#' numero de atributos: 33
#' numero de instancias: 1302
#' numero de instancias completas: 159
#' numero de valores faltosos: 7830
#' % instancias complestas: 12
#' 
#' url: https://openml2.win.tue.nl/d/930

df_original <- readFromCsv(df_locations[1], df_name)
df_original <- dropLevelFromDataframe(df_original)
df_original <- convertCategoricalToNumerical(df_original)

columns_to_normalize <- c(2:32)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(c(1, 2))
for(i in 2:32)
{
  convert_types[[i]] <- c(i, 3)
}
convert_lvls <- list(
  c(1, c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
         "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
         "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
         "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
         "UT", "VA", "VT", "WA", "WI", "WV", "WY"))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c(2, rep(1, 31))
