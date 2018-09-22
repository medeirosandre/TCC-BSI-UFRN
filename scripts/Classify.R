
#' inverter iterações sobre df_names com times_to_run.

classifiers_names <- c("J48")
classifiers <- list(J48)
times_to_run <- 30

classifier_mean_accuracy <- data.frame(matrix(
  data = 1,
  nrow = 1,
  ncol = length(techniques_sufix) + 2
))

classifiers_names_index <- 1
for(i in classifiers)
{
  for(j in df_names)
  {
    accuracy_final <- data.frame(matrix(
      1,
      nrow = 1,
      ncol = length(techniques_sufix) + 1
    ))
    for(k in 1:times_to_run)
    {
      accuracy_row_to_append <- data.frame(matrix(
        1,
        nrow = 1,
        ncol = length(techniques_sufix) + 1
      ))
      
      class <- as.formula("class ~ .")
      dados <- readFromCsv(df_locations[1], j, "")
      dados <- dropLevelFromDataframe(dados, "?")
      
      dados_indexes <- c(sample(as.integer(rownames(dados)), 
                                trunc(nrow(dados) * 0.75)))
      
      dados_train <- dados[dados_indexes, ]
      dados_test <- dados[-dados_indexes, ]
      
      dados_fit <- i(
        formula = class,
        data = dados_test,
        na.action = NULL
      )
      
      model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
      model_accuracy <- sum(diag(model$confusionMatrix)) /
        sum(model$confusionMatrix) * 100
      
      accuracy_row_to_append[,1] <- model_accuracy
      for(l in techniques_sufix)
      {
        class <- as.formula("class ~ .")
        dados <- readFromCsv(df_locations[2], j, l)
        dados <- dropLevelFromDataframe(dados, "?")
        
        dados_train <- dados[dados_indexes, ]
        dados_test <- dados[-dados_indexes, ]
        
        dados_fit <- i(
          formula = class,
          data = dados_test,
          na.action = NULL
        )
        
        model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
        model_accuracy <- sum(diag(model$confusionMatrix)) /
          sum(model$confusionMatrix) * 100
        
        accuracy_row_to_append[,(which(
          techniques_sufix == l) + 1)] <- model_accuracy
      }
      
      accuracy_final <- appendRowIntoDataframe(
        df_original = accuracy_final,
        row_to_append = accuracy_row_to_append
      )
    }
    
    accuracy_final <- accuracy_final[-1,]
    colnames(accuracy_final) <- c(
      "Original", techniques_sufix
    )
    
    writeToCsv(
      df.toWrite = accuracy_final,
      df.location = df_locations[3],
      df.name = classifiers_names[classifiers_names_index],
      df.sufix = paste("_", j, sep = "")
    )
  }
  classifiers_names_index <- classifiers_names_index + 1
}
