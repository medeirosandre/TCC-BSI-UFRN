
#' Hip Measurements:
#' 
#' nome: hip.csv
#' numero de classes: 2
#' numero de atributos: 8
#' numero de instancias: 54
#' numero de instancias completas: 24
#' numero de valores faltosos: 120
#' % instancias completas: 44
#' 
#' url: https://www.openml.org/d/898

df_name <- df_names[3]

df_original <- readFromCsv(df_locations[1], df_name, "")
df_original <- dropLevelFromDataframe(df_original, "?")
df_original <- convertCategoricalToNumerical(df_original)
# df_original <- dropInstancesWithFullNA(df_original)

df_noNA <- getCompleteCases(df_original)
df_onlyNA <- getIncompleteCases(df_original)

convert_types <- list(c(1, 3), c(2, 3), c(3, 3), c(4, 3),
                      c(5, 3), c(6, 3), c(7, 3))

convert_lvls <- list()

fill_na_using <- c(1,1,1,1,1,1,1)

df_final <- fillNAWithCompleteDatasetAppending(df_noNA, df_onlyNA, fill_na_using)
writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[1])

df_final <- fillNAWithCompleteDatasetNotAppending(df_noNA, df_onlyNA, fill_na_using)
writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[2])

df_final <- fillNAWithDatasetOfCasesClassAppending(df_noNA, df_onlyNA, fill_na_using)
writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[3])

df_final <- fillNAWithDatasetOfCasesClassNotAppending(df_noNA, df_onlyNA, fill_na_using)
writeToCsv(df_final, df_locations[2], df_name, techniques_sufix[4])

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
