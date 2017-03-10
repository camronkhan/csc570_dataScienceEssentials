# import data
Xy_train = read.csv("../../../kaggle_data/customer_retention/midterm_train.csv", stringsAsFactors = FALSE)
X_test = read.csv("../../../kaggle_data/customer_retention/midterm_test.csv", stringsAsFactors = FALSE)

# get missing value counts
Xy_train_missing <- sapply(Xy_train, function(x) { sum(is.na(x)) })
X_test_missing <- sapply(X_test, function(x) { sum(is.na(x)) })
Xy_train_missing
X_test_missing

# clean categorical data
cleanMonth <- function (x) {
  if (x == 'Jun') return ('june')
  else if (x == 'July') return ('july')
  else if (x == 'Aug') return ('august')
  else if (x == 'May') return ('may')
  else if (x == 'Mar') return ('march')
  else if (x == 'Apr') return ('april')
  else if (x == 'sept.') return ('september')
  else if (x == 'Feb') return ('february')
  else if (x == 'Oct') return ('october')
  else if (x == 'Nov') return ('november')
  else if (x == 'January') return ('january')
  else if (x == 'Dev') return ('december')
}

CleanWeekday <-