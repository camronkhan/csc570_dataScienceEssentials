install.packages("doParallel")
library(doParallel)
install.packages("caret")
library(caret)
install.packages("RANN")
library(RANN)

#=========================================================================================================================================

# MULTICORE PROCESSING

# register cores
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)

#=========================================================================================================================================

# IMPORT DATA

# import data
Xy_train = read.csv("../../../../kaggle_data/customer_retention/midterm_train.csv", stringsAsFactors = FALSE)
X_test = read.csv("../../../../kaggle_data/customer_retention/midterm_test.csv", stringsAsFactors = FALSE)

#=========================================================================================================================================

# CLEAN DATA

# bind data frames
Xy_test <- X_test
Xy_test$y <- NA
Xy_combined <- rbind(Xy_train, Xy_test)

# clean character data
cleanMonth <- function(x) {
  if (x == 'Jun') 'june'
  else if (x == 'July') 'july'
  else if (x == 'Aug') 'august'
  else if (x == 'May') 'may'
  else if (x == 'Mar') 'march'
  else if (x == 'Apr') 'april'
  else if (x == 'sept.') 'september'
  else if (x == 'Feb') 'february'
  else if (x == 'Oct') 'october'
  else if (x == 'Nov') 'november'
  else if (x == 'January') 'january'
  else if (x == 'Dev') 'december'
  else x
}
Xy_combined$month <- as.factor(sapply(Xy_combined$x19, cleanMonth))

getQuarter <- function(x) {
  if (x == 'Jun') 'Q2'
  else if (x == 'July') 'Q3'
  else if (x == 'Aug') 'Q3'
  else if (x == 'May') 'Q2'
  else if (x == 'Mar') 'Q1'
  else if (x == 'Apr') 'Q2'
  else if (x == 'sept.') 'Q3'
  else if (x == 'Feb') 'Q1'
  else if (x == 'Oct') 'Q4'
  else if (x == 'Nov') 'Q4'
  else if (x == 'January') 'Q1'
  else if (x == 'Dev') 'Q4'
  else x
}
Xy_combined$quarter <- as.factor(sapply(Xy_combined$x19, getQuarter))

getTemp <- function(x) {
  if (x == 'Jun') 'Warm'
  else if (x == 'July') 'Warm'
  else if (x == 'Aug') 'Warm'
  else if (x == 'May') 'Warm'
  else if (x == 'Mar') 'Cold'
  else if (x == 'Apr') 'Warm'
  else if (x == 'sept.') 'Warm'
  else if (x == 'Feb') 'Cold'
  else if (x == 'Oct') 'Cold'
  else if (x == 'Nov') 'Cold'
  else if (x == 'January') 'Cold'
  else if (x == 'Dev') 'Cold'
  else x
}
Xy_combined$temp <- as.factor(sapply(Xy_combined$x19, getTemp))

getSeason <- function(x) {
  if (x == 'Jun') 'Summer'
  else if (x == 'July') 'Summer'
  else if (x == 'Aug') 'Summer'
  else if (x == 'May') 'Spring'
  else if (x == 'Mar') 'Spring'
  else if (x == 'Apr') 'Spring'
  else if (x == 'sept.') 'Fall'
  else if (x == 'Feb') 'Winter'
  else if (x == 'Oct') 'Fall'
  else if (x == 'Nov') 'Fall'
  else if (x == 'January') 'Winter'
  else if (x == 'Dev') 'Winter'
  else x
}
Xy_combined$season <- as.factor(sapply(Xy_combined$x19, getSeason))

cleanWeekday <- function(x) {
  if (x == 'monday') 'monday'
  else if (x == 'tuesday') 'tuesday'
  else if (x == 'wednesday') 'wednesday'
  else if (x == 'thurday') 'thursday'
  else if (x == 'friday') 'friday'
  else x
}
Xy_combined$weekday <- as.factor(sapply(Xy_combined$x43, cleanWeekday))

cleanRegion <- function(x) {
  if (x == 'asia') 'asia'
  else if (x == 'euorpe') 'europe'
  else if (x == 'america') 'america'
  else x
}
Xy_combined$region <- as.factor(sapply(Xy_combined$x16, cleanRegion))

parseMoneyToFloat <- function(x) { as.double(gsub("[$,()\\s]", "", x)) }
Xy_combined$x44 <- sapply(Xy_combined$x44, parseMoneyToFloat)

parsePercentToFloat <- function(x) { as.double(sub("%", "", x)) / 100.00 }
Xy_combined$x09 <- sapply(Xy_combined$x09, parsePercentToFloat)

# split data into numeric and categorical
Xy_combined <- subset(Xy_combined, select = -c(x19, x43, x16))
Xy_combined_categorical <- subset(Xy_combined, select = c(temp, season, month, quarter, weekday, region))
Xy_combined_numeric <- subset(Xy_combined, select = -c(y, temp, season, month, quarter, weekday, region))

# get summary of data
#summary.factor(Xy_combined_categorical$temp)
#summary.factor(Xy_combined_categorical$season)
#summary.factor(Xy_combined_categorical$quarter)
#summary.factor(Xy_combined_categorical$month)
#summary.factor(Xy_combined_categorical$weekday)
#summary.factor(Xy_combined_categorical$region)
#summary(Xy_combined_numeric)
# 'season' seems to be a natural joint at which to cleave the month feature

# drop temp, quarter, and month features
Xy_combined <- subset(Xy_combined, select = -c(temp, month, quarter))

# separate X and y data
X_combined <- subset(Xy_combined, select = -c(y))
y_combined <- subset(Xy_combined, select = c(y))
X <- X_combined
y <- y_combined

# verify all features are numeric or factor
#str(X)

#=========================================================================================================================================

# PREPROCESS DATA

# get dummy variables
dummies <- dummyVars(" ~ .", data = X)
X <- data.frame(predict(dummies, newdata = X))
X <- subset(X, select = -c(region., weekday., season.))

# preprocess
X_pp <- preProcess(X, verbose=TRUE, method = c("center", "scale", "YeoJohnson", "knnImpute", "pca", "nzv", "zv"))
X_preprocessed <- predict(X_pp, newdata = X)

# test-train split
X_train <- X_preprocessed[1:160000,]
y_labels <- y[1:160000,]
y_labels <- ifelse(y_labels==1,'yes','no')
y_labels <- as.factor(y_labels)
X_test <- X_preprocessed[160001:200000,]

#=========================================================================================================================================

# TUNE & RUN MODEL

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE, allowParallel = TRUE)

xgbModel <- train(x = X_train, y = y_labels, method = "xgbTree", trControl = fitControl, tuneLength = 5, metric = "ROC")

plot(xgbModel, metric='ROC')
y_pred <- predict(xgbModel, newdata = X_test, type = "prob")
y_pred_numeric <- as.numeric(ifelse(y_labels=='yes', 1, 0))
write.csv(y_pred, "y_pred.csv")
print("DONE")



