# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Machine Learning Basics

## Basics of Evaluating Machine Learning Algorithms Comprehension Check

### Caret package, training and test sets, and overall accuracy

library(caret)
library(dslabs)

data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male", "Female"),length(test_index), replace = TRUE)

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
     factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
          factor(levels = levels(test_set$sex))
     mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy) %>% 
     ggplot(aes(cutoff, accuracy)) + 
     geom_point() + 
     geom_line() 

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
     factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

### Confusion Matrix

table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
     mutate(y_hat = y_hat) %>%
     group_by(sex) %>% 
     summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")
prev

mat <- matrix(c("True positives (TP)", "False negatives (FN)", 
                "False positives (FP)", "True negatives (TN)"), 2, 2)
colnames(mat) <- c("Actually Positive", "Actually Negative")
rownames(mat) <- c("Predicted positve", "Predicted negative")
as.data.frame(mat) %>% knitr::kable()

confusionMatrix(data = y_hat, reference = test_set$sex)

### Balanced accuracy and F1 score

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
          factor(levels = levels(test_set$sex))
     F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
     ggplot(aes(cutoff, F_1)) + 
     geom_point() + 
     geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
     factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)

### Prevalence matters in practice

### ROC and precision-recall curves

p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
     factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
     y_hat <- 
          sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Guessing",
          FPR = 1 - specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(FPR, TPR, color = method)) +
     geom_line() +
     geom_point() +
     xlab("1 - Specificity") +
     ylab("Sensitivity")

map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          cutoff = x, 
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
}) %>%
     ggplot(aes(FPR, TPR, label = cutoff)) +
     geom_line() +
     geom_point() +
     geom_text(nudge_y = 0.01)

guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), 
                     replace = TRUE, prob=c(p, 1-p)) %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Guess",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()

guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                     prob=c(p, 1-p)) %>% 
          factor(levels = c("Male", "Female"))
     list(method = "Guess",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Male", "Female"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()
