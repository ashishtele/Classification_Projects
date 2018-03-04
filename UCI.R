
################ CART ################
########### rpart ####################

rm(list = ls())

load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(caret)
  library(rpart)
  library(tree)
  library(MASS)
  library(kableExtra)
  library(knitr)
}


load_lb()

##data load

uci <- read.csv(file.choose(),header = F, sep = ",")
tbl_df(uci)

## Grow a tree

set.seed(1992)
model.tree <- rpart(V9~.-V9,data = uci, method = "class")
plot(model.tree, uniform=T)
text(model.tree,use.n = T)

## Prune a Tree

kable(model.tree$cptable)
#CP - values of the complexity parameter
#nsplit - no of splits
#xerror - cross validated classification error rate
#xstd - std dev of cross validation error

opt <- model.tree$cptable[which.min(model.tree$cptable[,"xerror"]),"CP"]
#opt - optimal complexity parameter

model.ptree <- prune(model.tree,cp = opt)

plot(model.ptree,uniform = T)
text(model.ptree, use.n = T)

summary(model.ptree)

