
runAnova <- function(result, tam) {
  number_rep <- rep(tam, (length(result) / tam))
  groups <- rep(1:length(number_rep), number_rep)
  data <- data.frame(result = result, groups = factor(groups))
  fit <- lm(result ~ groups, data)
  return (anova(fit))
}

for(i in classifiers_names)
{
  current_classifier_results <- readFromCsv(
    df.location = df_locations[3],
    df.name = i
  )
  current_classifier_results <- dropColumnFromDataFrame(
    df_to_drop_column = current_classifier_results,
    column_to_drop = 1
  )
  
  shapiro_results <- c(colnames(current_classifier_results))
  ks_results <- c(colnames(current_classifier_results))
  for(j in 1:ncol(current_classifier_results))
  {
    x <- current_classifier_results[[j]]
    d1 <- shapiro.test(x)
    d2 <- ks.test(x, "pnorm", mean(x), sd(x))
    shapiro_results <- c(shapiro_results, d1$p.value)
    ks_results <- c(ks_results, d2$p.value)
  }
  rm(j, x, d1, d2)
  
  writeToCsv(
    df.toWrite = matrix(
      data = shapiro_results,
      nrow = ncol(current_classifier_results)
      ),
    df.location = df_locations[5],
    df.name = i,
    df.sufix = "shappiro_test"
  )
  
  writeToCsv(
    df.toWrite = matrix(
      data = ks_results,
      nrow = ncol(current_classifier_results)
    ),
    df.location = df_locations[5],
    df.name = i,
    df.sufix = "ks_test"
  )
  rm(shapiro_results, ks_results)
  
  p_values_f <- c(colnames(current_classifier_results)[-1])
  p_values_t <- c(colnames(current_classifier_results)[-1])
  p_values_a <- c(colnames(current_classifier_results[-1]))
  for(j in 2:ncol(current_classifier_results))
  {
    x1 <- current_classifier_results[[1]]
    x2 <- current_classifier_results[[j]]
    xf <- as.matrix(current_classifier_results[, c(1, j)])
    d1 <- friedman.test(xf)
    d2 <- t.test(x1, x2)
    d3 <- runAnova(c(x1, x2), nrow(current_classifier_results))
    p_values_f <- c(p_values_f, d1$p.value)
    p_values_t <- c(p_values_t, d2$p.value)
    p_values_a <- c(p_values_a, d3$`Pr(>F)`[1])
  }
  rm(j, xf, d1)
  
  writeToCsv(
    df.toWrite = matrix(
      data = p_values_f,
      nrow = ncol(current_classifier_results) - 1
    ),
    df.location = df_locations[5],
    df.name = i,
    df.sufix = "friedman_test"
  )
  
  writeToCsv(
    df.toWrite = matrix(
      data = p_values_t,
      nrow = ncol(current_classifier_results) - 1
    ),
    df.location = df_locations[5],
    df.name = i,
    df.sufix = "t_test"
  )
  
  writeToCsv(
    df.toWrite = matrix(
      data = p_values_a,
      nrow = ncol(current_classifier_results) - 1
    ),
    df.location = df_locations[5],
    df.name = i,
    df.sufix = "anova_test"
  )
  rm(p_values_f, p_values_t)
}
rm(i)

rm(current_classifier_results)

for(i in classifiers_names)
{
  classifier_result <- readFromCsv(
    df_locations[4],
    i
  )[-1]
  colnames(classifier_result) <- as.character(seq(1:9))
  
  png(filename = paste("../", i, ".png", sep = ""))
  par(las = 1)
  boxplot(
    classifier_result,
    main = i,
    horizontal = F,
    col = c(2:8, 11,12)
  )
  dev.off()
}
rm(i)
