
# run on windows
# setwd("D:/workspace/UFRN/R")
# run on linux
# setwd("~/Workspace/ufrn/tcc")
# run on docker
setwd("/workdir/andre/tcc")

source("scripts/src/Functions.R")
source("scripts/src/Imports.R")

dir.create(file.path(getwd(), df_locations[2]), showWarnings = F)
dir.create(file.path(getwd(), df_locations[3]), showWarnings = F)

source("scripts/2year.R")
source("scripts/3year.R")
source("scripts/Adult.R")
source("scripts/AdultAutism.R")
source("scripts/APSFailure.R")
source("scripts/BreastCancer.R")
source("scripts/BreastCancerW.R")
source("scripts/CongretionalVoting.R")
source("scripts/CreditApproval.R")
source("scripts/Hip.R")
source("scripts/JungleChessRatPanther.R")
source("scripts/Kicks.R")
source("scripts/MammograghicMass.R")
source("scripts/Mice.R")
source("scripts/Mushroom.R")

# source("scripts/HCCSurvival.R")
# source("scripts/PrimaryTumor.R")

source("scripts/src/Classify.R")
