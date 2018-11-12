
# run on windows
# setwd("D:/workspace/UFRN/R")
# setwd("C:/Users/Arthur/Documents/andre/tcc")
# run on linux
setwd("~/Workspace/ufrn/tcc")
# run on docker
# setwd("/workdir/andre/tcc")

source("scripts/src/Functions.R")
source("scripts/src/Imports.R")

#' Imports packages and sets dataframes locations and names
installNeededPackages()

dir.create(file.path(getwd(), df_locations[2]), showWarnings = F)
dir.create(file.path(getwd(), df_locations[3]), showWarnings = F)
dir.create(file.path(getwd(), df_locations[4]), showWarnings = F)
dir.create(file.path(getwd(), df_locations[5]), showWarnings = F)

which_techniques_to_apply <- c()
for(i in df_names)
{
  techniques_completed <- getFileNames(
    path_to_look = df_locations[2],
    pattern = i
  )
  
  which_techniques_to_apply <- getWhichTechniquesToAplly(techniques_completed)
  
  if(length(which_techniques_to_apply) != 0)
  {
    df_name <- i
    source(paste("scripts/", i, ".R", sep = ""))
    source("scripts/src/ExecuteTechniques.R")
    
    rm(
      df_name, df_original, columns_to_normalize, convert_lvls, convert_types,
      fill_na_using
    )
  }
  
  rm(which_techniques_to_apply, techniques_completed)
}
rm(i)

classifiers_names <- c("J48", "IBk", "JRip")
classifiers <- list(J48, IBk, JRip)

# source("scripts/src/Classify.R")
source("scripts/src/FixDataset.R")
source("scripts/src/RunStatisticalAnalisys.R")

rm(classifiers_names, classifiers)
