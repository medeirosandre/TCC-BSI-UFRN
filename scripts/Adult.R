
#' Adult:
#' 
#' name do arquivo: adult.csv
#' numero de classes: 2
#' numero de atributos: 15
#' numero de instancias: 48842
#' numero de instancias completas: 45222
#' numero de valores fatosos: 6465
#' % instancias completas: 92
#' 
#' url: https://www.openml.org/d/1590

df_name <- df_names[5]

df_original <- readFromCsv(
  df.location = df_locations[1],
  df.name = df_name
)
df_original <- dropLevelFromDataframe(
  df.original = df_original,
  levelToDrop = "?"
)
df_original <- convertSpecificColumnsFromCatToNum(
  df_to_convert = df_original,
  columns_to_convert = c(1, 3, 5, 11, 12, 13)
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

# 1 = convertion from ordinal categorical data to numerical data
# 2 = convertion from categorical data to numerical data through binarization
# 3 = just append

convert_types <- list(
  c(1, 3), c(2, 2), c(3, 3), c(4, 1),
  c(5, 3), c(6, 2), c(7, 2), c(8, 2),
  c(9, 2), c(10, 1), c(11, 3), c(12, 3),
  c(13, 3), c(14, 2)
)

convert_lvls <- list(
  c(2, c(
    "Federal-gov", "Local-gov", "Never-worked", "Private", "Self-emp-inc", 
    "Self-emp-not-inc", "State-gov", "Without-pay"
  )),
  c(4, c(
    "Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th",
    "HS-grad", "Some-college", "Assoc-voc", "Assoc-acdm", "Bachelors",
    "Masters", "Prof-school", "Doctorate"
  )),
  c(6, c(
    "Divorced", "Married-AF-spouse", "Married-civ-spouse",
    "Married-spouse-absent", "Never-married", "Separated", "Widowed"
  )),
  c(7, c(
    "Adm-clerical", "Armed-Forces", "Craft-repair", "Exec-managerial", 
    "Farming-fishing", "Handlers-cleaners", "Machine-op-inspct",
    "Other-service", "Priv-house-serv", "Prof-specialty", "Protective-serv",
    "Sales", "Tech-support", "Transport-moving"
  )),
  c(8, c(
    "Husband", "Not-in-family", "Other-relative", "Own-child", "Unmarried",
    "Wife" 
  )),
  c(9, c(
    "Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black", "Other", "White"
  )),
  c(10, c("Female", "Male")),
  c(14, c(
    "Cambodia", "Canada", "China", "Columbia", "Cuba", "Dominican-Republic",
    "Ecuador", "El-Salvador", "England", "France", "Germany", "Greece", 
    "Guatemala", "Haiti", "Holand-Netherlands", "Honduras", "Hong", "Hungary",
    "India", "Iran", "Ireland", "Italy", "Jamaica", "Japan", "Laos", "Mexico",
    "Nicaragua", "Outlying-US(Guam-USVI-etc)", "Peru", "Philippines", "Poland",
    "Portugal", "Puerto-Rico", "Scotland", "South", "Taiwan", "Thailand",
    "Trinadad&Tobago", "United-States", "Vietnam", "Yugoslavia" 
  ))
)

# 1 = mean
# 2 = fashion

fill_na_using <- c(1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2)

# df_final <- fillNAWithCompleteDatasetAppending(
#   df.noNA = df_noNA,
#   df.onlyNA = df_onlyNA,
#   fill_na_using = fill_na_using
# )
# writeToCsv(
#   df.toWrite = df_final,
#   df.location = df_locations[2],
#   df.name = df_name,
#   df.sufix = techniques_sufix[1]
# )
# 
# df_final <- fillNAWithCompleteDatasetNotAppending(
#   df.noNA = df_noNA,
#   df.onlyNA = df_onlyNA,
#   fill_na_using = fill_na_using
# )
# writeToCsv(
#   df.toWrite = df_final,
#   df.location = df_locations[2],
#   df.name = df_name,
#   df.sufix = techniques_sufix[2]
# )

df_final <- fillNAWithDatasetOfCasesClassAppending(
  df.noNA = df_noNA,
  df.onlyNA = df_onlyNA,
  fill_na_using = fill_na_using
)
writeToCsv(
  df.toWrite = df_final,
  df.location = df_locations[2],
  df.name = df_name,
  df.sufix = techniques_sufix[3]
)

df_final <- fillNAWithDatasetOfCasesClassNotAppending(
  df.noNA = df_noNA,
  df.onlyNA = df_onlyNA,
  fill_na_using = fill_na_using
)
writeToCsv(
  df.toWrite = df_final,
  df.location = df_locations[2],
  df.name = df_name,
  df.sufix = techniques_sufix[4]
)

tech_suf <- c(techniques_sufix[5:8])
for(i in 1:4)
{
  df_final <- fillNAWithKNNFromCompleteDatasetAppending(
    df.noNA = df_noNA,
    df.onlyNA = df_onlyNA,
    convert.typs = convert_types,
    convert.lvls = convert_lvls,
    fillUsing = fill_na_using,
    numOfK = (i + i + 1)
  )
  writeToCsv(
    df.toWrite = df_final,
    df.location = df_locations[2],
    df.name = df_name,
    df.sufix = tech_suf[i]
  )
}

tech_suf <- c(techniques_sufix[9:12])
for(i in 1:4)
{
  df_final <- fillNAWithKNNFromCompleteDatasetNotAppending(
    df.noNA = df_noNA,
    df.onlyNA = df_onlyNA,
    convert.typs = convert_types,
    convert.lvls = convert_lvls,
    fillUsing = fill_na_using,
    numOfK = (i + i + 1)
  )
  writeToCsv(
    df.toWrite = df_final,
    df.location = df_locations[2],
    df.name = df_name,
    df.sufix = tech_suf[i]
  )
}

tech_suf <- c(techniques_sufix[13:16])
for(i in 1:4)
{
  df_final <- fillNAWithKNNFromDatasetOfCasesClassAppending(
    df.noNA = df_noNA,
    df.onlyNA = df_onlyNA,
    convert.typs = convert_types,
    convert.lvls = convert_lvls,
    fillUsing = fill_na_using,
    numOfK = (i + i + 1)
  )
  writeToCsv(
    df.toWrite = df_final,
    df.location = df_locations[2],
    df.name = df_name,
    df.sufix = tech_suf[i]
  )
}

tech_suf <- c(techniques_sufix[17:20])
for(i in 1:4)
{
  df_final <- fillNAWithKNNFromDatasetOfCasesClassNotAppending(
    df.noNA = df_noNA,
    df.onlyNA = df_onlyNA,
    convert.typs = convert_types,
    convert.lvls = convert_lvls,
    fillUsing = fill_na_using,
    numOfK = (i + i + 1)
  )
  writeToCsv(
    df.toWrite = df_final,
    df.location = df_locations[2],
    df.name = df_name,
    df.sufix = tech_suf[i]
  )
}
