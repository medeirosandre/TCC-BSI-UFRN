
accuracy_final <- data.frame(matrix(
  1,
  nrow = 1,
  ncol = length(techniques_sufix) + 2
  )
)
for(i in df_names)
{
  accuracy_row_to_append <- data.frame(matrix(
      1,
      nrow = 1,
      ncol = length(techniques_sufix) + 2
    )
  )
  
  class <- as.formula("class ~ .")
  dados <- readFromCsv(df_locations[1], i, "")
  dados <- dropLevelFromDataframe(dados, "?")
  
  dados_indexes <- c(sample(as.integer(rownames(dados)), trunc(nrow(dados) * 0.75)))
  
  dados_train <- dados[dados_indexes, ]
  dados_test <- dados[-dados_indexes, ]
  
  dados_fit <- J48(
    formula = class,
    data = dados_test,
    na.action = NULL
  )
  
  model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
  model_accuracy <- sum(diag(model$confusionMatrix)) / sum(model$confusionMatrix) * 100
  
  accuracy_row_to_append[,1] <- i
  accuracy_row_to_append[,2] <- model_accuracy
  for(j in techniques_sufix)
  {
    class <- as.formula("class ~ .")
    dados <- readFromCsv(df_locations[2], i, j)
    dados <- dropLevelFromDataframe(dados, "?")
    
    # dados_indexes <- c(sample(as.integer(rownames(dados)), trunc(nrow(dados) * 0.75)))
    
    dados_train <- dados[dados_indexes, ]
    dados_test <- dados[-dados_indexes, ]
    
    dados_fit <- J48(
      formula = class,
      data = dados_test,
      na.action = NULL
    )
    
    model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
    model_accuracy <- sum(diag(model$confusionMatrix)) / sum(model$confusionMatrix) * 100
    
    accuracy_row_to_append[,(which(techniques_sufix == j) + 2)] <- model_accuracy
  }
  
  accuracy_final <- appendRowIntoDataframe(accuracy_final, accuracy_row_to_append)
}

accuracy_final <- accuracy_final[-1,]
colnames(accuracy_final) <- c("Nome", "Original",
                              "T1", "T2", "T3", "T4",
                              "T5", "T6", "T7", "T8",
                              "T9", "T10", "T11", "T12",
                              "T13", "T14", "T15", "T16",
                              "T17", "T18", "T19", "T20")


writeToCsv(
  df.toWrite = accuracy_final,
  df.location = df_locations[3],
  df.name = "TabelaAcuracias",
  df.sufix = ""
)
