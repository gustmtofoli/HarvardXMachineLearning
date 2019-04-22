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
