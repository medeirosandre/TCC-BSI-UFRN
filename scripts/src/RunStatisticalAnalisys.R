
for(i in classifiers_names)
{
  classifier_results <- getFileNames(
    path_to_look = df_locations[4],
    pattern = i
  )

  current_classifier_results <- readFromCsv(
    df.location = df_locations[4],
    df.name = substr(classifier_results[1], 1, nchar(classifier_results[1])-4)
  )
  for(j in classifier_results[-c(1, length(classifier_results))])
  {
    current_result <- readFromCsv(
      df.location = df_locations[4],
      df.name = substr(j, 1, nchar(j)-4)
    )

    current_classifier_results <- appendRowIntoDataframe(
      df_original = current_classifier_results,
      row_to_append = current_result
    )
  }
  rm(j)
  
  if(i == "IBk")
  {
    statistical_result_nem_ibk <- posthoc.friedman.nemenyi.test(
      as.matrix(current_classifier_results))
    statistical_result_con_ibk <- posthoc.friedman.conover.test(
      as.matrix(current_classifier_results))
  }
  else if(i == "J48")
  {
    statistical_result_nem_j48 <- posthoc.friedman.nemenyi.test(
      as.matrix(current_classifier_results))
    statistical_result_con_j48 <- posthoc.friedman.conover.test(
      as.matrix(current_classifier_results))
  }
  else if(i == "JRip")
  {
    statistical_result_nem_jrip <- posthoc.friedman.nemenyi.test(
      as.matrix(current_classifier_results))
    statistical_result_con_jrip <- posthoc.friedman.conover.test(
      as.matrix(current_classifier_results))
  }
}
rm(i, current_classifier_results, current_result)

print(statistical_result_con_ibk$p.value[,1])
print(statistical_result_nem_ibk$p.value[,1])

print(statistical_result_con_j48$p.value[,1])
print(statistical_result_nem_j48$p.value[,1])

print(statistical_result_con_jrip$p.value[,1])
print(statistical_result_nem_jrip$p.value[,1])

# round(statistical_result_nem_ibk$p.value[,1], digits = 2)

# statistical_result$p.value[,1] < 0.05
# friedman.test(as.matrix(current_classifier_results))
# posthoc.friedman.conover.test(as.matrix(current_classifier_results))
# posthoc.friedman.nemenyi.test(as.matrix(current_classifier_results))
