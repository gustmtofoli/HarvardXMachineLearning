set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Q1 =========================================================

schools %>% top_n(10, score) %>% arrange(desc(score))

# ============================================================


# Q2 ===========================================================
set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools %>% top_n(-10,score) %>% arrange(desc(score)) %>%  summarise(median_size = median(size))

median(schools$size)

schools_best_10_score <- schools %>% top_n(10, score) %>% arrange(desc(score))

median(schools_best_10_score$size)

# ===============================================================================

# Q3 ============================================================================
schools_worst_10_score <- schools %>% top_n(10, score) %>% arrange(desc(score))

schools_worst_10_score <- head(arrange(schools, score), 10)

median(schools_worst_10_score$size)

# ===============================================================================

# Q5 ============================================================================
schools %>% ggplot(aes(x=size,y=score)) + geom_point()

overall <- mean(sapply(scores, mean))

alpha <- 25

schools$reg_score <- sapply(1:nrow(schools), function(i){
  reg_score <- sapply(scores[i], function(x) sum(x-overall))/(schools$size[i]+alpha)
  
})

schools <- schools %>% mutate(score_reg = score_reg)
schools %>% group_by(id) %>% summarize(mean_sr=mean(score_reg)) %>% top_n(10,mean_sr) %>% arrange(desc(mean_sr))

# ===============================================================================


alpha <- 128
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

schools <- schools %>% mutate(score_reg = score_reg)
schools %>% group_by(id) %>% summarize(mean_sr=mean(score_reg)) %>% top_n(10,mean_sr) %>% arrange(desc(mean_sr))


alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  
plot(rmse)

alpha <- alphas[which.min(rmse)] 
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

alphas <- seq(0,200,0.25)


alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  


# Q5
overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)] 


#Q7
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)] 

# ========================================================
# MATRIX FACTORIZATION

#Q1
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
sum(y) + sum(y_svd)

# =================================================
# CLUSTERING

# Q2
h <- hclust(d)
plot(h)

# Q3
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)
