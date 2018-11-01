
for(i in classifiers_names)
{
  classifier_results <- getFileNames(
    path_to_look = df_locations[3],
    pattern = i
  )
  for(j in classifier_results[-length(classifier_results)])
  {
    current_result <- readFromCsv(
      df.location = df_locations[3],
      df.name = substr(j, 1, nchar(j) - 4)
    )
    
    current_result <- fixDatasetKNN(current_result)
    colnames(current_result) <- c(
      "Original", "MediaEModaCompleto_A", "MediaEModaCompleto",
      "MediaEModaMesmaClasse_A", "MediaEModaMesmaClasse", "kNNCompleto_A",
      "kNNCompleto", "kNNMesmaClasse_A", "kNNMesmaClasse"
    )
    
    writeToCsv(
      df.toWrite = current_result,
      df.location = df_locations[4],
      df.name = j
    )
    
    rm(current_result)
  }
  rm(j, classifier_results)
  
  current_result <- readFromCsv(
    df.location = df_locations[3],
    df.name = i
  )
  
  current_result <- fixFinalDatasetKNN(current_result)
  colnames(current_result) <- c(
    "Dataset", "Original", "MediaEModaCompleto_A", "MediaEModaCompleto",
    "MediaEModaMesmaClasse_A", "MediaEModaMesmaClasse", "kNNCompleto_A",
    "kNNCompleto", "kNNMesmaClasse_A", "kNNMesmaClasse"
  )
  
  writeToCsv(
    df.toWrite = current_result,
    df.location = df_locations[4],
    df.name = i
  )
  
  rm(current_result)
}
rm(i)
