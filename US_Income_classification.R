
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(mlr))
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(library(mice))
  suppressPackageStartupMessages(require(xgboost))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(ggplot2))
}

load_lb()

# Importing the train and test files

train <- fread("E:/Study/R Projects/Common files/US_Income_train.csv",
               na.strings = c(""," ","?","NA",NA))
test <- fread("E:/Study/R Projects/Common files/US_Income_test.csv",
               na.strings = c(""," ","?","NA",NA))

glimpse(train)
# train data: rows- 199523 and variables-41
glimpse(test)
# train data: rows- 99762 and variables-41

## Test has same number of columns as that of train (including income_level)

# Distinct values in dependent variable in both train and test

unique(train$income_level)
unique(test$income_level)

## same values and only two distinct values

# Changing the target variable to binary

train %>%
  mutate(income_level = ifelse(income_level == "+50000",1,0)) -> train
unique(train$income_level)

test %>%
  mutate(income_level = ifelse(income_level == "-50000",0,1)) -> test
unique(test$income_level)

# checking the proportion of 1 and 0 in 'Income_level'

prop.table(table(train$income_level))*100

## Majority has proportion of ~94%.
## peroformance will be decided based on minority prediction

# combine the train and test for further modifications

train$train <- 1
test$train <- 0
data <- rbind(train,test)

# changing to factor and numeric columns

fct_col <- c(2:5,7,8:16,20:29,31:38,40,41)
data[,fct_col] <- lapply(data[,fct_col],factor)
glimpse(data)

col_class <- sapply(data, function(x) class(x))
num_col <- c()
ft_col <- c()

for (i in 1:length(col_class))
{
  if(col_class[[i]] == "factor")
  {
    ft_col[i] <- names(data)[i]
  }
  else 
  {
    num_col[i] <- names(data)[i]
  }  
 i=i+1 
}

num_col <- num_col[!is.na(num_col)]
ft_col <- ft_col[!is.na(ft_col)]

library(plotly)
tr <- data %>% filter(train==1)

p <- ggplot(tr, aes(x=age, y = ..density..)) +
  geom_histogram(fill="red",bins = 500) +
  geom_density()
p <- ggplotly(p)


p <- ggplot(tr, aes(x=capital_losses, y = ..density..)) +
  geom_histogram(fill="red",bins = 500) +
  geom_density()
p
## Too much right skewed
summary(tr$capital_losses)


ggplot(tr, aes(x=age, y=wage_per_hour)) +
  geom_point(aes(color = income_level))
## most of people with age < 20/25 have income = 0


ggplot(tr, aes(x=class_of_worker, fill=income_level)) +
  geom_bar(position = "dodge")


ggplot(tr, aes(x=education, fill=income_level)) +
  geom_bar(position = "dodge")
## Children have income 0
## Bachelor holders have largest proportion of 1

prop.table(table(tr$marital_status,tr$income_level))*100

## Data cleasing

# Correlation

corrl <- cor(tr[,num_col[-8]])
corrplot::corrplot(corrl, method = "ellipse", type = "upper")

rmv <- findCorrelation(x=corrl, cutoff = 0.7)
rmv <- num_col[7]

data$weeks_worked_in_year <- NULL

## Missing values

percent <- function(x, digits = 1, format = "f", ...) 
{
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Function to determine missing and empty values in columns
countMissing <- function(x,y)
{
  ## calculate counts
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  if (mode(x) == "numeric") missing1 = sum(x=="", na.rm=TRUE) else missing1 = 0
  missing <- sum(is.na(x)) + missing1
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat(" #         Column Name: ",y,"\n", sep="")
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}


for(i in 1:length(col_class))
{
  countMissing(tr[[i]],names(tr)[i])
}
#or
sort(sapply(tr, function(x) sum(is.na(x))),decreasing = TRUE)
sort(sapply(tr, function(x) sum(x=="")),decreasing = TRUE)

# removing columns
rmv1 <- c("migration_msa","migration_reg","migration_within_reg","migration_sunbelt")
data <- data[,!names(data) %in% rmv1]

# changing NA to Unavailable

rmv2 <- c("country_father","country_mother","country_self","hispanic_origin",
          "state_of_previous_residence")

data[,names(data) %in% rmv2] <- lapply(data[,names(data) %in% rmv2],as.character)

data$country_father[is.na(data$country_father)] <- "Unavailable"
data$country_mother[is.na(data$country_mother)] <- "Unavailable"
data$country_self[is.na(data$country_self)] <- "Unavailable"
data$hispanic_origin[is.na(data$hispanic_origin)] <- "Unavailable"
data$state_of_previous_residence[is.na(data$state_of_previous_residence)] <- "Unavailable"

data[,names(data) %in% rmv2] <- lapply(data[,names(data) %in% rmv2],factor)

#combine factor levels with less than 5% values

for(i in names(data[,names(data) %in% ft_col])){
  p <- 5/100
  ld <- names(which(prop.table(table(data[,names(data) %in% ft_col][[i]])) < p))
  levels(data[,names(data) %in% ft_col][[i]])[levels(data[,names(data) %in% ft_col][[i]]) %in% ld] <- "Other"
}

# Check the column levels
summarizeColumns(data[,names(data) %in% ft_col])[,"nlevs"]
summarizeColumns(data[,names(data) %in% ft_col])[,"nlevs"]


# bin age 

data <- data %>%
  mutate(age = cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old")))
data <- data %>% mutate(age = factor(age))

# bin other

data <- data %>%
  mutate(wage_per_hour = as.factor(ifelse(wage_per_hour == 0,"Zero","more")),
         capital_gains = as.factor(ifelse(capital_gains == 0,"Zero","more")),
         capital_losses = as.factor(ifelse(capital_losses == 0,"Zero","more")),
         dividend_from_Stocks = as.factor(ifelse(dividend_from_Stocks == 0,"Zero","more"))) 

## Modeling 

d_train <- data[data$train == 1,]
d_test <- data[data$train == 0,]

train_y <- d_train$income_level
test_y <- d_test$income_level
d_train$train <- NULL
d_test$train <- NULL

#One hot encodeing

d_train <- createDummyFeatures(d_train,target = "income_level")
d_test <- createDummyFeatures(d_test,target = "income_level")

# create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data = d_test, target = "income_level")

#remove zero variable features

train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

# data balancing

#undersampling 
train.under <- undersample(train.task,rate = 0.1) #only 10% of majority class
table(getTaskTargets(train.under))

#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

#SMOTE
train.smote <- smote(train.task,rate = 10,nn = 3)


View(listLearners("classif"))
getLearnerProperties(naive_lernr)

#naive-Bayes

naive_lernr <- makeLearner("classif.naiveBayes", predict.type = "response")
naive_lernr$par.vals <- list(laplace = 1)
  

# 10 CV
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)


#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_lernr,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}


fun_cv(train.task)
fun_cv(train.smote)


# train and predict
nb_mdl <- mlr::train(naive_lernr,train.smote)
nb_pred <- predict(nb_mdl, test.task)



#evaluation
nb_values <- nb_pred$data$response
confusionMatrix(d_test$income_level,nb_values)


## xgboost

set.seed(931992)

xgb_lrn <- makeLearner("classif.xgboost", predict.type = "response")
getLearnerProperties(xgb_lrn)

xgb_lrn$par.vals <- list(
                         objective = "binary:logistic",
                         eval_matric = "error",
                         nrounds = 150,
                         print_every_n = 50
                         )

#hyperparameter for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.3),
  makeNumericParam("eta", lower = 0.01, upper = 0.3),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
  )

# Search function
ran_control <- makeTuneControlRandom(maxit = 5L)

# 5 CV
set_cv <- makeResampleDesc("CV", iters = 5L, stratify = TRUE)

# Tune params

xgb_tune <- tuneParams(learner = xgb_lrn,
                       task = train.task,
                       resampling = set_cv,
                       measures = list(acc,tpr,fpr,tnr,fp,fn),
                       par.set = xg_ps,
                       control = ran_control)
xgb_tune


# set best parameters

xgb_new <- setHyperPars(learner = xgb_lrn, par.vals = xgb_tune$x)

# train model

xgmodel <- mlr::train(xgb_new, train.task)

# make prediction

pred.xg <- predict(xgmodel,test.task)

# predictions

xg_pred <- pred.xg$data$response

# confusion matrix

confusionMatrix(d_test$income_level,xg_pred)

# xgboost AUC

xgb_prob <- setPredictType(learner = xgb_new, predict.type = "prob")
xgmodel_prob <- mlr::train(xgb_prob, train.task)
pred.xgprob <- predict(xgmodel_prob, test.task)

# predicted probabilities
pred.xgprob$data[1:10,]

df <- generateThreshVsPerfData(pred.xgprob, measures = list(fpr,tpr))
plotROCCurves(df)


# Set threshold as 0.4

pred2 <- setThreshold(pred.xgprob,0.4)
confusionMatrix(d_test$income_level, pred2$data$response)

pred3 <- setThreshold(pred.xgprob,0.3)
confusionMatrix(d_test$income_level, pred3$data$response)


## SVM

getParamSet("classif.svm")

svm_learner <- makeLearner("classif.svm",predict.type = "response")
svm_learner$par.vals <- list(class.weights = c("0"=1,"1"=10),
                             kernel = "radial")

svm_param <- makeParamSet(
  makeIntegerParam("cost", lower = 0.1, upper = 100),
  makeIntegerParam("gamma", lower = 0.5, upper = 2)
)

# random search
set_srch <- makeTuneControlRandom(maxit=5L)

set_sv <- makeResampleDesc("CV",iters = 5L, stratify = TRUE)

# Tune params
svm_tune <- tuneParams(learner = svm_learner,
                       task = train.task,
                       measures = list(acc,tpr,tnr,fpr,fp,fn), 
                       par.set = svm_param,
                       control = set_srch,
                       resampling = set_sv)



#set hyperparameters
svm_new <- setHyperPars(learner = svm_learner, par.vals = svm_tune$x)

#train model
svm_model <- train(svm_new,train.task)

#test model
predict_svm <- predict(svm_model,test.task)

confusionMatrix(d_test$income_level,predict_svm$data$response)



