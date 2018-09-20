
#' Credit Approval
#' 
#' nome do arquivo: credit-approval.csv
#' numero de classes: 2
#' numero de atributos: 16
#' numero de instancias: 640
#' numero de instancias completas: 653
#' numero de valores faltosos: 67
#' % instancias completas: 94
#' 
#' url: https://www.openml.org/d/29

#' Original column names as seen in: https://rpubs.com/kuhnrl30/CreditScreen
#' 
#' 1 Male          : num  1 1 0 0 0 0 1 0 0 0 ...
#' 2 Age           : chr  "58.67" "24.50" "27.83" "20.17" ...
#' 3 Debt          : num  4.46 0.5 1.54 5.62 4 ...
#' 4 Married       : chr  "u" "u" "u" "u" ...
#' 5 BankCustomer  : chr  "g" "g" "g" "g" ...
#' 6 EducationLevel: chr  "q" "q" "w" "w" ...
#' 7 Ethnicity     : chr  "h" "h" "v" "v" ...
#' 8 YearsEmployed : num  3.04 1.5 3.75 1.71 2.5 ...
#' 9 PriorDefault  : num  1 1 1 1 1 1 1 1 1 0 ...
#' 0 Employed      : num  1 0 1 0 0 0 0 0 0 0 ...
#' 1 CreditScore   : num  6 0 5 0 0 0 0 0 0 0 ...
#' 2 DriversLicense: chr  "f" "f" "t" "f" ...
#' 3 Citizen       : chr  "g" "g" "g" "s" ...
#' 4 ZipCode       : chr  "00043" "00280" "00100" "00120" ...
#' 5 Income        : num  560 824 3 0 0 ...
#' 6 Approved      : chr  "+" "+" "+" "+" ...

df_name <- df_names[4]

df_original <- readFromCsv(
  df.location = df_locations[1],
  df.name = df_name,
  df.sufix = ""
)
df_original <- dropLevelFromDataframe(
  df.original = df_original,
  levelToDrop = "?"
)
df_original <- convertSpecificColumnsFromCatToNum(
  df_to_convert = df_original,
  columns_to_convert = c(2, 3, 8, 11, 14, 15)
)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

convert_types <- list(
  c(1, 2), c(2, 3), c(3, 3), c(4, 2),
  c(5, 2), c(6, 1), c(7, 2), c(8, 3),
  c(9, 1), c(10, 1), c(11, 3), c(12, 1),
  c(13, 2), c(14, 3), c(15, 3)
)

convert_lvls <- list(
  c(1, c("b", "a")),
  c(4, c("u", "y", "l", "t")),
  c(5, c("g", "p", "gg")),
  c(6, c("c", "d", "cc", "i", "j", "k", "m", 
         "r", "q", "w", "x", "e", "aa", "ff")),
  c(7, c("v", "h", "bb", "j", "n", "z", "dd", "ff", "o")),
  c(9, c("t", "f")),
  c(10, c("t", "f")),
  c(12, c("t", "f")),
  c(13, c("g", "p", "s"))
)

fill_na_using <- c(2,1,1,2,2,2,2,1,2,2,1,2,2,1,1)

df_final <- fillNAWithCompleteDatasetAppending(
  df.noNA = df_noNA,
  df.onlyNA = df_onlyNA,
  fill_na_using = fill_na_using
)
writeToCsv(
  df.toWrite = df_final,
  df.location = df_locations[2],
  df.name = df_name,
  df.sufix = techniques_sufix[1]
)

df_final <- fillNAWithCompleteDatasetNotAppending(
  df.noNA = df_noNA,
  df.onlyNA = df_onlyNA,
  fill_na_using = fill_na_using
)
writeToCsv(
  df.toWrite = df_final,
  df.location = df_locations[2],
  df.name = df_name,
  df.sufix = techniques_sufix[2]
)

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
