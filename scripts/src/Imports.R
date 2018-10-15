
#' Imports packages and sets dataframes locations and names

installNeededPackages()

df_locations <- c(
  "originalDatasets/", "cleanDatasets/", "classifierResults/"
)

# df_names <- c(
#   "2Year", "3Year", "Adult", "AdultAutism", "APSFailure", "BreastCancer",
#   "BreastCancerW", "CongretionalVoting", "CreditApproval", "Hip",
#   "JungleChessRatPanther", "Kicks", "MammographicMass", "Mice", "Mushroom"
# )

# df_names <- c(
#   "2Year", "3Year", "Adult", "AdultAutism", "BreastCancer", "BreastCancerW",
#   "CongretionalVoting", "CreditApproval", "Hip", "JungleChessRatPanther",
#   "MammographicMass", "Mice", "Mushroom", "Kicks", "APSFailure"
# )

# df_names <- c("Hip", "CreditApproval")

df_names <- c(
  "1Year", "2Year", "3Year", "4Year", "Adult", "AdultAutism", "BreastCancer", 
  "BreastCancerW", "CongretionalVoting", "CreditApproval", "Hip",
  "JungleChessRatPanther", "MammographicMass", "Mice", "Mushroom"
)

techniques_sufix <- c(
  "_MediaEModaConjuntoCompletoAppending",
  "_MediaEModaConjuntoCompletoNotAppending",
  "_MediaEModaConjuntoDeMesmaClasseAppending",
  "_MediaEModaConjuntoDeMesmaClasseNotAppending",
  "_MediaEModa3NNConjuntoCompletoAppending",
  "_MediaEModa5NNConjuntoCompletoAppending",
  "_MediaEModa7NNConjuntoCompletoAppending",
  "_MediaEModa9NNConjuntoCompletoAppending",
  "_MediaEModa3NNConjuntoCompletoNotAppending",
  "_MediaEModa5NNConjuntoCompletoNotAppending",
  "_MediaEModa7NNConjuntoCompletoNotAppending",
  "_MediaEModa9NNConjuntoCompletoNotAppending",
  "_MediaEModa3NNConjuntoDeMesmaClasseAppending",
  "_MediaEModa5NNConjuntoDeMesmaClasseAppending",
  "_MediaEModa7NNConjuntoDeMesmaClasseAppending",
  "_MediaEModa9NNConjuntoDeMesmaClasseAppending",
  "_MediaEModa3NNConjuntoDeMesmaClasseNotAppending",
  "_MediaEModa5NNConjuntoDeMesmaClasseNotAppending",
  "_MediaEModa7NNConjuntoDeMesmaClasseNotAppending",
  "_MediaEModa9NNConjuntoDeMesmaClasseNotAppending"
)
