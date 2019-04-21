library(dslabs)
data("movielens")
library(lubridate)

# Q1 =====================================================================

sqrt_n_ratings <- movielens %>% group_by(title,year) %>% summarise(sqrtcount = sqrt(n()))

sqrt_n_ratings %>% ggplot(aes(group=year,x=year, y=sqrtcount)) + geom_boxplot()

medians <- sqrt_n_ratings %>% group_by(year) %>% summarise(median = median(sqrtcount))
# =========================================================================

# Q2 ======================================================================

ssr <- movielens %>% filter(title == "Shawshank Redemption, The") %>% select(rating)
mean(ssr$rating)

fg <- movielens %>% filter(title == "Forrest Gump")
NROW(fg)/(2018-1994)

# =========================================================================

movielens <- mutate(movielens, date = as_datetime(timestamp))

movielens %>% mutate(week=wday(round_date(date, unit = "day"))) %>% 
  group_by(week) %>% summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(x=week, y = mean_rating)) + geom_line()

movielens %>% group_by(genres) %>% 
  summarise(n_ratings = n(), mean_rating = mean(rating), sd_rating = sd(rating)) %>% 
  filter(n_ratings>1000) %>% ggplot(aes(x=genres,y=mean_rating)) + geom_boxplot()

