library(caret)
library(dslabs)
library(dplyr)
library(lubridate)

# =====================================================================================
mnist_data = read_mnist()

# How many features are available to us for prediction in the mnist digits dataset?
ncol(mnist_data$train$images)

y <- mnist_data$train$labels
(y[5] + y[6])
(y[5] > y[6])

# ======================================================================================


# ======================================================================================

data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & 
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & 
                         hour(date_time) == 8 & between(minute(date_time), 15, 30),
                       "inclass", "online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type



# primeira pergunta
dat_inclass <- subset(dat, type == 'inclass')
dat_online <- subset(dat, type == 'online')

dat_inclass_female <- subset(dat_inclass, sex == 'Female')
proportion1 <- nrow(dat_inclass_female)/nrow(dat_inclass)
dat_online_female <- subset(dat_online, sex == 'Female')

dat_inclass_male <- subset(dat_inclass, sex == 'Male')
proportion2 <- nrow(dat_online_female)/nrow(dat_online)
dat_online_male <- subset(dat_online, sex == 'Male')

# segunda pergunta



# ======================================================================================

# ======================================================================================

library(caret)
library(tidyverse)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = .5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

foo <- function(x) {
  rangedValues <- seq(range(x)[1], range(x)[2], by = 0.1)
  acc <- sapply(rangedValues, function(i) {
    y_hat <- ifelse(x > i, 'virginica', 'versicolor')
    mean(y_hat == train$Species)
  })
  print(qplot(rangedValues, acc, geom = c('line', 'point')))
  acc
}
predictions <- apply(train[ , -5], 2, foo)
sapply(predictions, max)

## Overall accuracy for the test data
values <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2], by = 0.1)
smart_cutoff <- values[which.max(predictions$Petal.Length)]
smart_cutoff

y_hat <- ifelse(test$Petal.Length > smart_cutoff, "virginica", "versicolor")
mean(y_hat == test$Species)


## Check to see which feature will optimize our predictions, using the test data
predictions_w <- apply(test[ , -5], 2, foo)
sapply(predictions, max)

## Check overall accuracy using a combination of petal length and petal width
values_w <- seq(range(train$Petal.Width)[1], range(train$Petal.Width)[2], by = 0.1)
smart_cutoff_w <- values_w[which.max(predictions_w$Petal.Width)]
smart_cutoff_w

y_hat <- ifelse(
  (test$Petal.Length > smart_cutoff | test$Petal.Width > smart_cutoff_w),
  'virginica',
  'versicolor'
)
mean(y_hat == test$Species)

## Correct result obtained but here is the given solution for the exercise
## Note that it starts afresh, but this is totally unnecessary
data('iris')
iris <- iris[-which(iris$Species == 'setosa'), ]
y <- iris$Species

plot(iris, pch = 21, bg = iris$Species)

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index, ]
train <- iris[-test_index, ]

petalLengthRange <-
  seq(range(train[, 3])[1], range(train[, 3])[2], by = 0.1)
petalWidthRange <-
  seq(range(train[, 4])[1], range(train[, 4])[2], by = 0.1)
cutoffs <- expand.grid(petalLengthRange, petalWidthRange)

id <- sapply(seq(nrow(cutoffs)), function(i) {
  y_hat <-
    ifelse(train[, 3] > cutoffs[i, 1] |
             train[, 4] > cutoffs[i, 2], 'virginica', 'versicolor')
  mean(y_hat == train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id, ] %>% as.numeric
y_hat <-
  ifelse(test[, 3] > optimalCutoff[1] &
           test[, 4] > optimalCutoff[2],
         'virginica',
         'versicolor')
mean(y_hat == test$Species)


# ======================================================================================


