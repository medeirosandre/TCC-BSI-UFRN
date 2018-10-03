
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
    cat("Dataset:", df_name, " Technique: 3a  K:", (i + i + 1), "\n")
    
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
    cat("Dataset:", df_name, " Technique: 3b  K:", (i + i + 1), "\n")
    
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
    cat("Dataset:", df_name, " Technique: 4a  K:", (i + i + 1), "\n")
    
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
    cat("Dataset:", df_name, " Technique: 4b  K:", (i + i + 1), "\n")
    
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

cat("\n")
cat("Dataset:", df_name, " Technique: 1a", "\n")
executeTechnique1A()

cat("Dataset:", df_name, " Technique: 1b", "\n")
executeTechnique1B()

cat("Dataset:", df_name, " Technique: 2a", "\n")
executeTechnique2A()

cat("Dataset:", df_name, " Technique: 2b", "\n")
executeTechnique2B()

cat("\n")
executeTechnique3A()
cat("\n")
executeTechnique3B()
cat("\n")
executeTechnique4A()
cat("\n")
executeTechnique4B()
