# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Distance, Knn, Cross-validation, and Generative Models

## Cross-validation

### k-fold Cross-validation

### Bootstrap

n <- 10^6
income <- 10^(rnorm(n, 4.656786, 0.4394738))
qplot(log10(income), bins = 30, color = I("black"))

# seed was not set so values may be different
m <- median(income)
m

set.seed(1)
N <- 250
X <- sample(income, N)
M <- median(X)
M

library(gridExtra)
B <- 10^4
Ms <- replicate(B, {
     X <- sample(income, N)
     M <- median(X)
})
p1 <- qplot(Ms, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(Ms)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(Ms)
sd(Ms)

B <- 10^4
M_stars <- replicate(B, {
     X_star <- sample(X, N, replace = TRUE)
     M_star <- median(X_star)
})

tibble(monte_carlo = sort(Ms), bootstrap = sort(M_stars)) %>% 
     qplot(monte_carlo, bootstrap, data = .) + 
     geom_abline()

# seed was not set so values may be different
quantile(Ms, c(0.05, 0.95))
quantile(M_stars, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(Ms) + 1.96 * sd(Ms) * c(-1,1)
mean(M_stars) + 1.96 * sd(M_stars) * c(-1, 1)