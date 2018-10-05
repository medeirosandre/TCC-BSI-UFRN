
# run on windows
# setwd("D:/workspace/UFRN/R")
# run on linux
setwd("~/Workspace/ufrn/tcc")
# run on docker
# setwd("/workdir/andre/tcc")

source("scripts/src/Functions.R")
source("scripts/src/Imports.R")

dir.create(file.path(getwd(), df_locations[2]), showWarnings = F)
dir.create(file.path(getwd(), df_locations[3]), showWarnings = F)

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
    source(paste("scripts/", i, ".R", sep = ""))
    source("scripts/src/ExecuteTechniques.R")
  }
}

source("scripts/src/Classify.R")
