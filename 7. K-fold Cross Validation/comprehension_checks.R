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
# devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

# Q3
tt_stat_sig <- tt[tt$p.value <= 0.01,]
ind <- which(tt_stat_sig$p.value <= 0.01)
ind
length(ind)

# Q4
x_subset <- x[, ind]
logit_model_2 <- train(x_subset, y, method = 'glm')
logit_model_2$results

# Q5
data.frame(k = seq(101, 301, 25))
knn_model <- train(x_subset, y, method = 'knn', tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(knn_model)
knn_model$results

# Q6
# we use the entire dataset to select the columns used in the model.

# Q7
data("tissue_gene_expression")
tge <- tissue_gene_expression
k <- data.frame(k = seq(15, 51, 2))
knn_fit <- train(tge$x, tge$y, method = 'knn', tuneGrid = k)
ggplot(knn_fit)

# ==============================================

# BOOTSTRAP

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

#################### Question 3 ######################
set.seed(1)
y <- rnorm(100, 0, 1)
B <- 10000
quantile(y, 0.75)
qnorm(0.75)

N_quantile_75 <- replicate(B, {
  bootstrap_sample <- rnorm(100, 0, 1)
  quantile_75 <- quantile(bootstrap_sample, 0.75)
  
})
mean(N_quantile_75)
sd(N_quantile_75)
#####################################################

##################### Question 4 ####################
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
bootstrap_sample <- createResample(y, 10)

bs_10 <- sapply(bootstrap_sample, function(index){
  y_bs <- y[index]
  q_bs <- quantile(y_bs, 0.75)
  
})
mean(bs_10)
sd(bs_10)
#####################################################

#################### Question 5 #####################
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
bootstrap_sample <- createResample(y, 10000)

bs_10 <- sapply(bootstrap_sample, function(index){
  y_bs <- y[index]
  q_bs <- quantile(y_bs, 0.75)
  
})
mean(bs_10)
sd(bs_10)
####################################################
