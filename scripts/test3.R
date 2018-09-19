
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
  
  dados <- readFromCsv(df_locations[1], i, "")
  dados <- dropLevelFromDataframe(dados, "?")
  class <- as.formula("class ~ .")
  
  model <- J48(
    formula = class,
    data = dados,
    na.action = NULL
  )
  
  model_accuracy <- sum(diag(table(dados$class, predict(model)))) / 
    sum(table(dados$class, predict(model))) * 100
  
  accuracy_row_to_append[,1] <- i
  accuracy_row_to_append[,2] <- model_accuracy
  for(j in techniques_sufix)
  {
    dados <- readFromCsv(df_locations[2], i, j)
    class <- as.formula("class ~ .")
    
    model <- J48(
      formula = class,
      data = dados,
      na.action = NULL
    )
    
    model_accuracy <- sum(diag(table(dados$class, predict(model)))) / 
      sum(table(dados$class, predict(model))) * 100
    
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

