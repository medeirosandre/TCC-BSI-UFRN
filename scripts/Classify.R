
classifiers_names <- c("J48")
classifiers <- list(J48)
times_to_run <- 30

classifiers_names_index <- 1
for(i in classifiers)
{
  for(j in 1:times_to_run)
  {
    accuracy_final <- data.frame(matrix(
      1,
      nrow = 1,
      ncol = length(techniques_sufix) + 2
    ))
    for(k in df_names)
    {
      accuracy_row_to_append <- data.frame(matrix(
        1,
        nrow = 1,
        ncol = length(techniques_sufix) + 2
      ))
      
      class <- as.formula("class ~ .")
      dados <- readFromCsv(df_locations[1], k, "")
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
      
      accuracy_row_to_append[,1] <- k
      accuracy_row_to_append[,2] <- model_accuracy
      for(l in techniques_sufix)
      {
        class <- as.formula("class ~ .")
        dados <- readFromCsv(df_locations[2], k, l)
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
          techniques_sufix == l) + 2)] <- model_accuracy
      }
      
      accuracy_final <- appendRowIntoDataframe(
        df_original = accuracy_final,
        row_to_append = accuracy_row_to_append
      )
    }
    
    accuracy_final <- accuracy_final[-1,]
    colnames(accuracy_final) <- c(
      "Nome", "Original", techniques_sufix
    )
    
    writeToCsv(
      df.toWrite = accuracy_final,
      df.location = df_locations[3],
      df.name = "TabelaAcuracias_",
      df.sufix = paste(
        j, classifiers_names[classifiers_names_index], sep = "_")
    )
  }
  classifiers_names_index <- classifiers_names_index + 1
}
# 
# for(i in 1:30)
# {
#   accuracy_final <- data.frame(matrix(
#     1,
#     nrow = 1,
#     ncol = length(techniques_sufix) + 2
#   )
#   )
#   for(j in df_names)
#   {
#     accuracy_row_to_append <- data.frame(matrix(
#       1,
#       nrow = 1,
#       ncol = length(techniques_sufix) + 2
#     )
#     )
# 
#     class <- as.formula("class ~ .")
#     dados <- readFromCsv(df_locations[1], j, "")
#     dados <- dropLevelFromDataframe(dados, "?")
# 
#     dados_indexes <- c(sample(as.integer(rownames(dados)),
#                               trunc(nrow(dados) * 0.75)))
# 
#     dados_train <- dados[dados_indexes, ]
#     dados_test <- dados[-dados_indexes, ]
# 
#     dados_fit <- J48(
#       formula = class,
#       data = dados_test,
#       na.action = NULL
#     )
# 
#     model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
#     model_accuracy <- sum(diag(model$confusionMatrix)) /
#       sum(model$confusionMatrix) * 100
# 
#     accuracy_row_to_append[,1] <- j
#     accuracy_row_to_append[,2] <- model_accuracy
#     for(k in techniques_sufix)
#     {
#       class <- as.formula("class ~ .")
#       dados <- readFromCsv(df_locations[2], j, k)
#       dados <- dropLevelFromDataframe(dados, "?")
# 
#       dados_train <- dados[dados_indexes, ]
#       dados_test <- dados[-dados_indexes, ]
# 
#       dados_fit <- J48(
#         formula = class,
#         data = dados_test,
#         na.action = NULL
#       )
# 
#       model <- evaluate_Weka_classifier(dados_fit, newdata = dados_test)
#       model_accuracy <- sum(diag(model$confusionMatrix)) /
#         sum(model$confusionMatrix) * 100
# 
#       accuracy_row_to_append[,(which(
#         techniques_sufix == k) + 2)] <- model_accuracy
#     }
# 
#     accuracy_final <- appendRowIntoDataframe(
#       df_original = accuracy_final,
#       row_to_append = accuracy_row_to_append
#     )
#   }
# 
#   accuracy_final <- accuracy_final[-1,]
#   colnames(accuracy_final) <- c(
#     "Nome", "Original", techniques_sufix
#   )
# 
#   writeToCsv(
#     df.toWrite = accuracy_final,
#     df.location = df_locations[3],
#     df.name = "TabelaAcuracias",
#     df.sufix = paste("_", i, sep = "")
#   )
# }
