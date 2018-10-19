
executeTechnique1A <- function()
{
  df_final <- fillNAWithDataset(
    df_to_convert = df_original,
    fill_using = fill_na_using,
    appending = T
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
  df_final <- fillNAWithDataset(
    df_to_convert = df_original,
    fill_using = fill_na_using
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
  df_final <- fillNAWithDataset(
    df_to_convert = df_original,
    fill_using = fill_na_using,
    appending = T,
    same_class = T
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
  df_final <- fillNAWithDataset(
    df_to_convert = df_original,
    fill_using = fill_na_using,
    same_class = T
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
  start_of_k <- getStartOfKVector(
    path_to_look = df_locations[2],
    base_name = df_name,
    pattern = "NNConjuntoCompletoAppending"
  )
  tech_suf <- c(techniques_sufix[5:8])
  # for(i in 1:4)
  for(i in start_of_k:4)
  {
    cat("Dataset:", df_name, " Technique: 3a  K:", (i + i + 1), "\n")
    
    df_final <- fillNAWithKNN(
      df_to_complete = df_original,
      convert_typs = convert_types,
      convert_lvls = convert_lvls,
      columns_to_normalize = columns_to_normalize,
      fill_using = fill_na_using,
      num_of_k = (i + i + 1),
      appending = T
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
  start_of_k <- getStartOfKVector(
    path_to_look = df_locations[2],
    base_name = df_name,
    pattern = "NNConjuntoCompletoNotAppending"
  )
  tech_suf <- c(techniques_sufix[9:12])
  # for(i in 1:4)
  for(i in start_of_k:4)
  {
    cat("Dataset:", df_name, " Technique: 3b  K:", (i + i + 1), "\n")
    
    df_final <- fillNAWithKNN(
      df_to_complete = df_original,
      convert_typs = convert_types,
      convert_lvls = convert_lvls,
      columns_to_normalize = columns_to_normalize,
      fill_using = fill_na_using,
      num_of_k = (i + i + 1)
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
  start_of_k <- getStartOfKVector(
    path_to_look = df_locations[2],
    base_name = df_name,
    pattern = "NNConjuntoDeMesmaClasseAppending"
  )
  tech_suf <- c(techniques_sufix[13:16])
  # for(i in 1:4)
  for(i in start_of_k:4)
  {
    cat("Dataset:", df_name, " Technique: 4a  K:", (i + i + 1), "\n")
    
    df_final <- fillNAWithKNN(
      df_to_complete = df_original,
      convert_typs = convert_types,
      convert_lvls = convert_lvls,
      columns_to_normalize = columns_to_normalize,
      fill_using = fill_na_using,
      num_of_k = (i + i + 1),
      appending = T,
      same_class = T
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
  start_of_k <- getStartOfKVector(
    path_to_look = df_locations[2],
    base_name = df_name,
    pattern = "NNConjuntoDeMesmaClasseNotAppending"
  )
  tech_suf <- c(techniques_sufix[17:20])
  # for(i in 1:4)
  for(i in start_of_k:4)
  {
    cat("Dataset:", df_name, " Technique: 4b  K:", (i + i + 1), "\n")
    
    df_final <- fillNAWithKNN(
      df_to_complete = df_original,
      convert_typs = convert_types,
      convert_lvls = convert_lvls,
      columns_to_normalize = columns_to_normalize,
      fill_using = fill_na_using,
      num_of_k = (i + i + 1),
      same_class = T
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

if(1 %in% which_techniques_to_apply)
{
  cat("Dataset:", df_name, " Technique: 1a", "\n")
  executeTechnique1A()
}

if(2 %in% which_techniques_to_apply)
{
  cat("Dataset:", df_name, " Technique: 1b", "\n")
  executeTechnique1B()
}

if(3 %in% which_techniques_to_apply)
{
  cat("Dataset:", df_name, " Technique: 2a", "\n")
  executeTechnique2A()
}

if(4 %in% which_techniques_to_apply)
{
  cat("Dataset:", df_name, " Technique: 2b", "\n")
  executeTechnique2B()
}

if(5 %in% which_techniques_to_apply)
{
  cat("\n")
  executeTechnique3A()
}

if(6 %in% which_techniques_to_apply)
{
  cat("\n")
  executeTechnique3B()
}

if(7 %in% which_techniques_to_apply)
{
  cat("\n")
  executeTechnique4A()
}

if(8 %in% which_techniques_to_apply)
{
  cat("\n")
  executeTechnique4B()
}
