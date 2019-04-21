models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

set.seed(1)
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

acc <- sapply(fits, function(x){
  y_hat <- predict(x, newdata = mnist_27$test)
  accuracy <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
})

length(mnist_27$test$y)
length(models)

accuracy <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["accuracy"]

y_hat <- sapply(fits, function(x){
  y_hat <- predict(x, newdata = mnist_27$test)
})

y_pred <- factor(apply(y_hat,1,function(x){
  names(sort(table(x),decreasing=TRUE))[1]
}
))


accuracy <- confusionMatrix(data = y_pred, reference = mnist_27$test$y)$overall["Accuracy"]

est_acc <- sapply(fits, function(x){
  min(x$results$Accuracy)
})

y_pred_est <- factor(apply(y_hat[,est_acc>0.8],1,function(x){
  names(sort(table(x),decreasing=TRUE))[1]
}
))

accuracy <- confusionMatrix(data = y_pred_est, reference = mnist_27$test$y)$overall["Accuracy"]

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

pca <- as.data.frame(prcomp(tissue_gene_expression$x)$x)
pca$tissue <- tissue_gene_expression$y
pca %>% ggplot(aes(x=pca$PC1, y=pca$PC2, color = tissue)) +
  geom_point()

ave <- apply(tissue_gene_expression$x,1,mean)
pca$ave <- ave

pca %>% ggplot(aes(x=pca$PC1,y=pca$ave, color=tissue)) +
  geom_point()

x <- tissue_gene_expression$x
x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

pca %>% ggplot(aes(x=pca$tissue,y=pca$PC7)) + geom_boxplot()
