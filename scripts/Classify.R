
classifiers_names <- c("J48", "IBk", "JRip")
classifiers <- list(J48, IBk, JRip)
times_to_run <- 30

classifiers_names_index <- 1
for(i in classifiers)
{
  print(classifiers_names[classifiers_names_index])
  classifier_mean_accuracy <- data.frame(matrix(
    data = 1,
    nrow = 1,
    ncol = length(techniques_sufix) + 2
  ))
  
  for(j in df_names)
  {
    cat("\n")
    accuracy_final <- data.frame(matrix(
      data = 1,
      nrow = 1,
      ncol = length(techniques_sufix) + 1
    ))
    
    classifier_mean_accuracy_row <- data.frame(matrix(
      data = 2,
      nrow = 1,
      ncol = length(techniques_sufix) + 2
    ))
    for(k in 1:times_to_run)
    {
      if(k %% 3 == 0)
      {
        cat(
          "Classifier:", classifiers_names[classifiers_names_index],
          " Dataset:", j,
          " Run:", k,
          "\n"
        )
      }
      
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
      
      if(classifiers_names[classifiers_names_index] == "IBk")
      {
        dados_fit <- i(
          formula = class,
          data = dados_test,
          na.action = NULL,
          control = Weka_control(K = trunc(sqrt(nrow(dados))))
        )
      }
      else
      {
        dados_fit <- i(
          formula = class,
          data = dados_test,
          na.action = NULL
        )
      }
      
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
        
        if(classifiers_names[classifiers_names_index] == "IBk")
        {
          dados_fit <- i(
            formula = class,
            data = dados_test,
            na.action = NULL,
            control = Weka_control(K = trunc(sqrt(nrow(dados))))
          )
        }
        else
        {
          dados_fit <- i(
            formula = class,
            data = dados_test,
            na.action = NULL
          )
        }
        
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
    
    classifier_mean_accuracy_row[,1] <- j
    for(m in 1:ncol(accuracy_final))
    {
      classifier_mean_accuracy_row[,m + 1] <- mean(accuracy_final[,m])
    }

    classifier_mean_accuracy <- appendRowIntoDataframe(
      df_original = classifier_mean_accuracy,
      row_to_append = classifier_mean_accuracy_row
    )
    
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
  
  classifier_mean_accuracy <- classifier_mean_accuracy[-1,]
  
  colnames(classifier_mean_accuracy) <- c(
    "Base", "Original", techniques_sufix
  )
  
  writeToCsv(
    df.toWrite = classifier_mean_accuracy,
    df.location = df_locations[3],
    df.name = classifiers_names[classifiers_names_index],
    df.sufix = ""
  )
  
  classifiers_names_index <- classifiers_names_index + 1
}
