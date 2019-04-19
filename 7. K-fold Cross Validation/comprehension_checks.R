library(dslabs)
library(tidyverse)
library(caret)
library(dslabs)

# Q1
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# fit <- train(x_subset, y)
fit <- train(x_subset, y, method="glm")
fit$results

# Q2
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

# ==============================================

# bootstrap

# Q1
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
resample1 <- indexes$Resample01
count <- 0
for (element in resample1) {
  if (element == 4) {
    count = count + 1
  }
}
count

# Q2
count <- 0
for (resample in indexes){
  for (element in resample) {
    if (element == 3) {
      count = count + 1
    }
  } 
}
count

# Q3
set.seed(1)
y <- rnorm(100, 0, 1)
