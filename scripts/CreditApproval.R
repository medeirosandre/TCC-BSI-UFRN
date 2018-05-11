
##############################
###    Credit Approval     ###
##############################

setwd("f:/workspace/ufrn/r")

# imports
##############################
source("tcc/scripts/src/Imports.R")
source("tcc/scripts/src/Functions.R")
##############################

# create dataframes
##############################
dataframe.original <- read.csv(paste(dataframes.location[1], 
                                     dataframes.names[2], 
                                     ".csv", sep = ""), 
                               stringsAsFactors = TRUE)
dataframe.clean <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))
dataframe.default <- dataframe.original
##############################

# removing NAs from dataFrame.default
##############################
levels(dataframe.default[[1]]) <- list(a="a", b="b", ZZZ="")
levels(dataframe.default[[4]]) <- list(l="l", u="u", y="y", ZZZ="")
levels(dataframe.default[[5]]) <- list(g="g", gg="gg", p="p", ZZZ="")
levels(dataframe.default[[6]]) <- list(aa="aa", c="c", cc="cc",
                                       d="d", e="e", ff="ff",
                                       i="i", j="j", k="k",
                                       m="m", q="q", r="r",
                                       w="w", x="x", ZZZ="")
levels(dataframe.default[[7]]) <- list(bb="bb", dd="dd", ff="ff",
                                       h="h", j="j", n="n",
                                       o="o", v="v", z="z",
                                       ZZZ="")
for(i in 1:(ncol(dataframe.default)-1))
{
  if(is.numeric(dataframe.default[[i]]))
  {
    dataframe.default[[i]][is.na(dataframe.default[[i]])] <- 0
  }
}
##############################

# creating dataFrame.clean
##############################
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 1))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 4))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 5))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 6))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 7))

dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 9))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 10))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 12))
dataframe.clean <- cbind(dataframe.clean, convertColCatToNum(dataframe.default, 13))

for(i in 1:ncol(dataframe.clean))
{
  dataframe.clean[[i]] <- as.numeric(dataframe.clean[[i]])
}

for(i in 1:(ncol(dataframe.default)-1))
{
  if(is.numeric(dataframe.default[[i]]))
  {
    dataframe.clean[length(dataframe.clean)+1] <- scale(dataframe.default[[i]])
  }
}
##############################

# retrieving class
##############################
dataframe.clean <- dataframe.clean[,-1]
dataframe.clean[length(dataframe.clean)+1] <- dataframe.default$Class
##############################

# filling NAs and scalling data
##############################
dataframe.final <- fillNAs(dataframe.original, dataframe.clean, dataframe.default, 10)
for (i in 1:ncol(dataframe.final))
{
  if(is.numeric(dataframe.final[[i]]))
  {
    dataframe.final[[i]] <- scale(dataframe.final[[i]])
  }
}
##############################

# binarizind data
##############################
x <- data.frame(matrix(1, nrow = nrow(dataframe.original), 1))

levels(dataframe.final[[1]]) <- list(a="a", b="b", ZZZ="")
levels(dataframe.final[[4]]) <- list(l="l", u="u", y="y", ZZZ="")
levels(dataframe.final[[5]]) <- list(g="g", gg="gg", p="p", ZZZ="")
levels(dataframe.final[[6]]) <- list(aa="aa", c="c", cc="cc",
                                       d="d", e="e", ff="ff",
                                       i="i", j="j", k="k",
                                       m="m", q="q", r="r",
                                       w="w", x="x", ZZZ="")
levels(dataframe.final[[7]]) <- list(bb="bb", dd="dd", ff="ff",
                                       h="h", j="j", n="n",
                                       o="o", v="v", z="z",
                                       ZZZ="")

# x <- cbind(x, convertColCatToNum(dataframe.final, 1))
# x <- x[,-length(x)]
# x <- cbind(x, convertColCatToNum(dataframe.final, 4))
# x <- x[,-length(x)]
# x <- cbind(x, convertColCatToNum(dataframe.final, 5))
# x <- x[,-length(x)]
# x <- cbind(x, convertColCatToNum(dataframe.final, 6))
# x <- x[,-length(x)]
# x <- cbind(x, convertColCatToNum(dataframe.final, 7))
# x <- x[,-length(x)]

x <- cbind(x,convertColCatToNumOrd(dataframe.final, 1, c("b", "a", "ZZZ")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 4, c("u", "y", "l", "t", "ZZZ")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 5, c("g", "p", "gg", "ZZZ")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 6, c("c", "d", "cc",
                                                         "i", "j", "k",
                                                         "m", "r", "q",
                                                         "w", "x", "e",
                                                         "aa", "ff", "ZZZ")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 7, c("v", "h", "bb",
                                                         "j", "n", "z",
                                                         "dd", "ff", "o",
                                                         "ZZZ")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 9, c("t", "f")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 10, c("t", "f")))
x <- cbind(x,convertColCatToNumOrd(dataframe.final, 12, c("t", "f")))

x <- cbind(x, convertColCatToNumOrd(dataframe.final, 13, c("g", "p", "s")))

for(i in 1:ncol(dataframe.final))
{
  if(is.numeric(dataframe.final[[i]]))
  {
    x[length(x)+1] <- dataframe.final[[i]]
  }
}

x <- x[,-1]
x[[length(x)+1]] <- dataframe.final[[length(dataframe.final)]]

##############################

# write dataFrame to file
##############################
write.csv(x, paste(dataframes.location[2],
                                 dataframes.names[2],".csv", sep = ""),
          na = "", row.names = FALSE)
##############################
