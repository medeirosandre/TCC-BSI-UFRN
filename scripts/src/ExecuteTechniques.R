
executeTechnique1A <- function()
{
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
}

executeTechnique1B <- function()
{
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
}

executeTechnique2A <- function()
{
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
}

executeTechnique2B <- function()
{
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
}

executeTechnique3A <- function()
{
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
}

executeTechnique3B <- function()
{
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
}

executeTechnique4A <- function()
{
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
}

executeTechnique4B <- function()
{
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
}

executeTechnique1A()
executeTechnique1B()
executeTechnique2A()
executeTechnique2B()
executeTechnique3A()
executeTechnique3B()
executeTechnique4A()
executeTechnique4B()
