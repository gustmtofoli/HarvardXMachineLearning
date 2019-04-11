# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Distance, Knn, Cross-validation, and Generative Models

## Nearest Neighbors

### Distance

library(tidyverse)
library(dslabs)

set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

sqrt(sum((x_1 - x_2)^2))

sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

d <- dist(x)
class(d)

as.matrix(d)[1:3,1:3]

image(as.matrix(d))

image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

### Knn

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

?knn3

knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

### Overtraining and Oversmoothing

plot_cond_prob <- function(p_hat=NULL){
     tmp <- mnist_27$true_p
     if(!is.null(p_hat)){
          tmp <- mutate(tmp, p=p_hat)
     }
     tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
          geom_raster(show.legend = FALSE) +
          scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
          stat_contour(breaks=c(0.5),color="black")
}
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
     ggtitle("kNN-5 estimate")
library(gridExtra)
grid.arrange(p1, p2, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

p1 <- mnist_27$true_p %>% 
     mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
     ggplot() +
     geom_point(data = mnist_27$train, aes(x_1, x_2, color= y),
                pch=21, show.legend = FALSE) +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
     ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
     mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
     ggplot() +
     geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
                pch=21, show.legend = FALSE) +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
     ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
     ggtitle("Logistic regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
     ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
     fit <- knn3(y ~ ., data = mnist_27$train, k = k)
     
     y_hat <- predict(fit, mnist_27$train, type = "class")
     cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
     train_error <- cm_train$overall["Accuracy"]
     
     y_hat <- predict(fit, mnist_27$test, type = "class")
     cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
     test_error <- cm_test$overall["Accuracy"]
     
     tibble(train = train_error, test = test_error)
})
accuracy %>% mutate(k = ks) %>%
     gather(set, accuracy, -k) %>%
     mutate(set = factor(set, levels = c("train", "test"))) %>%
     ggplot(aes(k, accuracy, color = set)) + 
     geom_line() +
     geom_point() 

p1 <- plot_cond_prob() + ggtitle("True conditional probability")
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 41)
p2 <- plot_cond_prob(predict(knn_fit, newdata = mnist_27$true_p)[,2]) +
     ggtitle("kNN-41 estimate")
grid.arrange(p2, p1, nrow=1)

max(accuracy$test)
