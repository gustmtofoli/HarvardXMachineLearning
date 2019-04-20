# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Classification with More than Two Classes and the Caret Package

## Classification with More than Two Classes

### Trees Motivation

rafalib::mypar()
x <- seq(0,1,len=100)
y <- rep(1, 100)
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
lines(x[c(15,35)], y[c(15,35)], col="blue",lwd=3)
points(x,y, cex = 0.25)
points(x[25],y[25],col="blue", cex = 0.5, pch=4)
text(x[c(15,35)], y[c(15,35)], c("[","]"))

tmp <- expand.grid(1:10, 1:10)
x <- tmp[,1]
y <- tmp[,2]
rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-0.5, x[25]-0.5, x[25]+0.5, x[25]+0.5),
        c(y[25]-0.5, y[25]+0.5, y[25]+0.5, y[25]-0.5), col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-sqrt(10)/2, x[25]-sqrt(10)/2, x[25]+sqrt(10)/2, x[25]+sqrt(10)/2),
        c(y[25]-sqrt(10)/2, y[25]+sqrt(10)/2, y[25]+sqrt(10)/2, y[25]-sqrt(10)/2),
        col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

library(tidyverse)
p <- 1:100
qplot(p, .1^(1/p), ylim = c(0,1))

### Classification and Regression Trees (CART)

library(tidyverse)
library(dslabs)
data("olive")
olive %>% tbl_df

table(olive$region)

olive <- select(olive, -area)

library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

olive %>% gather(fatty_acid, percentage, -region) %>%
     ggplot(aes(region, percentage, fill = region)) +
     geom_boxplot() +
     facet_wrap(~fatty_acid, scales = "free")

p <- olive %>% 
     ggplot(aes(eicosenoic, linoleic, color = region)) + 
     geom_point()
p

p + geom_vline(xintercept = 0.065, lty = 2) + 
     geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

library(caret)
library(rpart)
train_rpart <- train(region ~ ., method = "rpart", data = olive)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

fit <- rpart(margin ~ ., data = polls_2008,
             control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
     mutate(y_hat = predict(pruned_fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)


plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

polls_2008 %>% 
     mutate(y_hat = predict(train_rpart)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

### Classification (Decision) Trees

library(dslabs)
data("mnist_27")

train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

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
p1 <- plot_cond_prob(predict(train_rpart, newdata = mnist_27$true_p, type = "prob")[,2]) +
     ggtitle("Decision Tree")
p1

### Random Forests

library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
     mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_line(aes(day, y_hat), col="red")

library(rafalib)
# output .gif file
animation::saveGIF({
     set.seed(1)
     ntrees <- 50
     XLIM <- range(polls_2008$day)
     YLIM <- range(polls_2008$margin)
     
     sum <- rep(0,nrow(polls_2008))
     res <- vector("list", ntrees)
     
     for(i in 0:ntrees){
          mypar(1,1)
          if(i==0){
               with(polls_2008, plot(day, margin, pch = 1, main = "Data", xlim=XLIM,
                                     ylim=YLIM,
                                     xlab = "Days", ylab="Obama - McCain"))
          } else{
               ind <- sort(sample(1:nrow(polls_2008), replace = TRUE))
               tmp <- polls_2008[ind,]
               fit <- rpart(margin~day, data = tmp)
               pred <- predict(fit, newdata = tmp)
               res[[i]] <- data_frame(day = tmp$day, margin=pred)
               pred <- predict(fit, newdata = polls_2008)
               sum <- sum+pred
               avg <- sum/i
               with(tmp, plot(day,margin, pch=1, xlim=XLIM, ylim=YLIM, type="n",
                              xlab = "Days", ylab="Obama - McCain",
                              main=ifelse(i==1, paste(i, "tree"),paste(i, "trees"))))
               for(j in 1:i){
                    with(res[[j]], lines(day, margin, type="s", col="grey", lty=2))
               }
               with(tmp, points(day,margin, pch=1))
               with(res[[i]], lines(day, margin, type="s",col="azure4",lwd=2))
               lines(polls_2008$day, avg, lwd=3, col="blue")
          }
     }
     for(i in 1:5){
          mypar(1,1)
          with(polls_2008, plot(day, margin, pch = 1, main="Final", xlim=XLIM, ylim=YLIM,
                                xlab = "Days", ylab="Obama - McCain"))
          lines(polls_2008$day, avg, lwd=3, col="blue")
     }
}, movie.name = "rf.gif", ani.loop=0, ani.delay =50)
# plot
{set.seed(1)
ntrees <- 50
XLIM <- range(polls_2008$day)
YLIM <- range(polls_2008$margin)
sum <- rep(0,nrow(polls_2008))
res <- vector("list", ntrees)
mypar(2,3)
show <- c(1, 5, 25, 50) 
for(i in 0:ntrees){
     if(i==0){
          with(polls_2008, plot(day, margin, pch = 1, main = "Data", xlim=XLIM,
                                ylim=YLIM,
                                xlab = "Days", ylab="Obama - McCain"))
     } else{
          ind <- sort(sample(1:nrow(polls_2008), replace = TRUE))
          tmp <- polls_2008[ind,]
          fit <- rpart(margin~day, data = tmp)
          pred <- predict(fit, newdata = tmp)
          res[[i]] <- data_frame(day = tmp$day, margin=pred)
          pred <- predict(fit, newdata = polls_2008)
          sum <- sum+pred
          avg <- sum/i
          if(i %in% show){
               with(tmp, plot(day,margin, pch=1, xlim=XLIM, ylim=YLIM, type="n",
                              xlab = "Days", ylab="Obama - McCain",
                              main=ifelse(i==1, paste(i, "tree"),paste(i, "trees"))))
               for(j in 1:i){
                    with(res[[j]], lines(day, margin, type="s", col="grey", lty=2))
               }
               with(tmp, points(day,margin, pch=1))
               with(res[[i]], lines(day, margin, type="s",col="azure4",lwd=2))
               lines(polls_2008$day, avg, lwd=3, col="blue")
          }
     }
}
with(polls_2008, plot(day, margin, pch = 1, main="Final", xlim=XLIM, ylim=YLIM,
                      xlab = "Days", ylab="Obama - McCain"))
lines(polls_2008$day, avg, lwd=3, col="blue")}

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_rf, newdata = mnist_27$true_p, type = "prob")[,2]) +
     ggtitle("Random Forest")
p1

train_rf_2 <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2,
                                   minNode = c(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

p2 <- plot_cond_prob(predict(train_rf_2, newdata = mnist_27$true_p, type="prob")[,2]) +
     ggtitle("Random Forest")
p2
