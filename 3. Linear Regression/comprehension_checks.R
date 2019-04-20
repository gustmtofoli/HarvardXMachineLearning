### Comprehension Check: Linear Regression
library(caret)
library(tidyverse)

## Question 1
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69,69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
cal_RMSE <- replicate(n = 100,
                      {
                        test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
                        train_set <- dat %>% slice(-test_index)
                        test_set <- dat %>% slice(test_index)
                        
                        linear_model <- lm(y ~ x, data = train_set)
                        y_hat <- predict(linear_model, test_set)
                        RMSE <- sqrt(mean((y_hat - test_set$y)^2))
                      })
mean(cal_RMSE)
sd(cal_RMSE)



















# ============================================================================

# Question 1

y_rmse <- c(1:100)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat

set.seed(1)
for(i in 1:100) {    
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  # Train linear model
  fit <- lm(y ~ x, data = train_set)
  fit
  
  # Loss Function
  y_hat <- predict(fit, test_set)
  y_rmse[i] <- sqrt(mean((y_hat - test_set$y)^2))
  print(i)
}

y_rmse
mean(y_rmse)
sd(y_rmse)

## Question 2
set.seed(1)
cal_RMSE <- function(size) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69,69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
  RMSEs <- replicate(n = 100,
                     {
                       test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
                       train_set <- dat %>% slice(-test_index)
                       test_set <- dat %>% slice(test_index)
                       
                       linear_model <- lm(y ~ x, data = train_set)
                       y_hat <- predict(linear_model, test_set)
                       RMSE <- sqrt(mean((y_hat - test_set$y)^2))
                     })
  
  cat("the mean RMSE for n =", size, "is ", mean(RMSEs), "\n")
  cat("the sd RMSE for n =", size, "is ", sd(RMSEs), "\n", "\n")
}

n <- c(100, 500, 1000, 5000, 10000)
sapply(n, cal_RMSE)


# Question 4

## Question 4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
cal_RMSE <- replicate(n = 100,
                      {
                        test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
                        train_set <- dat %>% slice(-test_index)
                        test_set <- dat %>% slice(test_index)
                        linear_model <- lm(y ~ x, data = train_set)
                        y_hat <- predict(linear_model, test_set)
                        RMSE <- sqrt(mean((y_hat - test_set$y)^2))
                      })
mean(cal_RMSE)
sd(cal_RMSE)
histogram(cal_RMSE)

# Question 6
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train linear model
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_12 <- lm(y ~ x_1 + x_2, data = train_set)

# Loss Functions
y_hat_1 <- predict(fit_1, test_set)
RMSE_1 <- sqrt(mean((y_hat_1 - test_set$y)^2))

y_hat_2 <- predict(fit_2, test_set)
RMSE_2 <- sqrt(mean((y_hat_2 - test_set$y)^2))

y_hat_12 <- predict(fit_12, test_set)
RMSE_12 <- sqrt(mean((y_hat_12 - test_set$y)^2))  



# ========================================================

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

delta <- seq(0, 3, len=25)



## Question 8
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
  