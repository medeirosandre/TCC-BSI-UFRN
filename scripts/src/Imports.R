
#' Imports packages and sets dataframes locations and names

installNeededPackages()

df_locations <- c(
  "tcc/originalDatasets/", "tcc/cleanDatasets/", "tcc/classifierResults/"
)

df_names <- c(
  "vote", "mushroom", "hip", "credit-approval", "adult", "breast-cancer",
  "mice", "primary-tumor", "mammographic-mass", "hcc-survival", "aps-failure",
  "kick", "jngl-chs-2pcs-ndgm-rat-pntr", "autism-adult", "breast-w", "2year",
  "3year"
)

# df_names <- c("hip")

# df_names <- c(
#   "adult", "breast-cancer", "credit-approval", "hcc-survival", "hip",
#   "mammographic-mass", "mice", "mushroom", "vote"
# )

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
