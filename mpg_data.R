

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

data <- read.csv("E:\\Study\\R Projects\\Common files/auto-mpg.csv")
head(data)

str(data)

# Column split
data %>% separate(car.name,c("Company","Model","Type")) -> data

glimpse(data)


# Changing datatype as per requirement
data$cylinders <- as.factor(data$cylinders)
data$horsepower <- as.numeric(data$horsepower)
data$weight <- as.numeric(data$weight)
data$origin <- as.factor(data$origin)
data$model.year <- as.factor(data$model.year)

glimpse(data)

## Selecting required columns

data$Model <- NULL
data$Type <- NULL

tbl_df(data)

## checking and changing car names
sort(unique(data$Company))

data$Company <- gsub("chevy","chevrolet",data$Company)
data$Company <- gsub("chevroelt","chevrolet",data$Company)
data$Company <- gsub("maxda","mazda",data$Company)
data$Company <- gsub("vokswagen","volkswagen",data$Company)
data$Company <- gsub("toyouta","toyota",data$Company)
data$Company <- gsub("vw","volkswagen",data$Company)
data$Company <- as.factor(data$Company)

str(data)

## Checking NA in data

countMissing <- function(x) {
  ## calculate counts
  missing = sum(is.na(x))
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat("#           TOTAL ROWS: ", totalRows, "\n", sep="")
  cat("# Missing Values (NAs): ", missing, "\n", sep="")
  cat("# Empty Strings (\"\"): ", emptyStrings, "\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, "\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}

colnm <- colnames(data)

for (i in 1:length(colnm))
{ 
  cat("col name:",colnm[i], "\n",sep = "")
  countMissing(as.character(paste("data",colnm[i],sep="$")))
}

summary(data)

## Checking distribution

ggplot(data, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.1, fill = "red") +
  xlab("mpg") + 
  ylab("Count") +
  ggtitle("Distribution: mpg") 


ggplot(data, aes(x = acceleration)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.1, fill = "green") +
  xlab("Acceleration") + 
  ylab("Count") +
  ggtitle("Distribution: acceleration") 

#No of cars

data %>% group_by(Company) %>% summarise(count = n()) %>% arrange(desc(count)) -> cnt_car
cnt_car

ggplot(cnt_car,aes(x= reorder(cnt_car$Company,cnt_car$count), y= cnt_car$count)) +
  geom_bar( stat = "identity", fill = "blue") + 
  coord_flip()

#No of cylinders

data %>% group_by(cylinders) %>% summarise(count = n()) %>% arrange(desc(count)) -> cnt_cyl
cnt_cyl

ggplot(cnt_cyl, aes(x= cylinders,y= count))+
  geom_bar( stat = "identity", fill =  "blue") 

## mpg vs cylinders

ggplot(data, aes(x=cylinders, y= mpg, fill = cylinders)) +
  geom_boxplot() 

## correlation

pairs(data)

## set seed
set.seed(931992)

intrain <- createDataPartition(y= data$mpg, p = 0.75, list = F)
training <- data[intrain,]
testing <- data[-intrain,]

# stepwise regression

mdl_step <- lm(mpg~.,data = data)
step <- stepAIC(mdl_step, direction = "both")

summary(step)
step$anova

plot(step)

## Prediction

pred_train <- predict(step, training)
pred_test <- predict(step, testing)

rsme_train <- sqrt(sum((pred_train - training$mpg)^2))
rsme_test <- sqrt(sum((pred_test - testing$mpg)^2))

## Regression tree

mdl_tree <- rpart(mpg~., data = training)
print(mdl_tree)
summary(mdl_tree)

cp_tbl <- data.frame(mdl_tree$cptable)
class(cp_tbl)

cp_bst <- cp_tbl[which.min(cp_tbl$xerror),"CP"]

plotcp(mdl_tree)

tree_p <- prune(mdl_tree, cp = cp_bst)
plot(tree_p, uniform = T)
text(tree_p, use.n = T)
summary(tree_p)
