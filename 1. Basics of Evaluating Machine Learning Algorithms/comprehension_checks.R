library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(purrr)
# library(tidyverse)

# =====================================================================================
mnist_data = read_mnist()

# How many features are available to us for prediction in the mnist digits dataset?
ncol(mnist_data$train$images)

y <- mnist_data$train$labels
(y[5] + y[6])
(y[5] > y[6])

# ======================================================================================


# ======================================================================================
# Comprehension Check: Confusion Matrix
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



# Q1
dat_inclass <- subset(dat, type == 'inclass')
dat_online <- subset(dat, type == 'online')

dat_inclass_female <- subset(dat_inclass, sex == 'Female')
proportion1 <- nrow(dat_inclass_female)/nrow(dat_inclass)
dat_online_female <- subset(dat_online, sex == 'Female')

dat_inclass_male <- subset(dat_inclass, sex == 'Male')
proportion2 <- nrow(dat_online_female)/nrow(dat_online)
dat_online_male <- subset(dat_online, sex == 'Male')

# Q2: Prediction using type
y_hat <- ifelse(x == "inclass", "Female", "Male")
mean(y == y_hat)

# Q3
table(y_hat, y)

# Q4: senitivity
# Q5: specificity
# Q6: prevalence of females
confusionMatrix(data = factor(y_hat), reference = y)


# ======================================================================================

# ======================================================================================




data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = .5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# iris_versicolor
iris_versicolor <- iris %>% filter(Species == "versicolor")
mean(iris_versicolor[["Petal.Length"]])
mean(iris_versicolor[,"Petal.Width"])
mean(iris_versicolor[["Sepal.Length"]])
mean(iris_versicolor[,"Sepal.Width"])

# iris_virginica
iris_virginica <- iris %>% filter(Species == "virginica")
mean(iris_virginica[["Petal.Length"]])
mean(iris_virginica[,"Petal.Width"])
mean(iris_virginica[["Sepal.Length"]])
mean(iris_virginica[,"Sepal.Width"])

cutoff <- seq(3.0, 8.0, by = 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

# ======================================================================================


