
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
}

posthoc.friedman.conover.test(as.matrix(current_classifier_results))
posthoc.friedman.nemenyi.test(as.matrix(current_classifier_results))

