# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Smoothing

### Introduction to Smoothing

library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
     mutate(resid = resid) %>% 
     ggplot(aes(day, margin)) + 
     geom_smooth(method = "lm", se = FALSE, color = "black") +
     geom_point(aes(color = resid), size = 3)

### Bin Smoothing and Kernels

span <- 3.5
tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) 
tmp %>% filter(center %in% c(-125, -55)) %>%
     ggplot(aes(day, margin)) +   
     geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
     geom_point(size = 2) +    
     geom_smooth(aes(group = center), 
                 method = "lm", formula=y~1, se = FALSE) +
     facet_wrap(~center)

# May have to download old version of gganimate
# Install using code below, then restart R session:
# library(devtools)
# install_github("thomasp85/gganimate", ref = "v0.1.1")
library(gganimate)
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="box", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>% 
     ggplot() +
     geom_smooth(aes(day, margin, group = center, frame = center), method = "lm", formula=y~1, se = FALSE) +
     geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(day, margin, frame = center)) +
     geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
     ggtitle("x0 = ")
gganimate(p, interval= .1)

span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")

x_0 <- -125
data.frame(x = polls_2008$day) %>% mutate(w_0 = 1*I(abs(x - x_0)<=span/2)) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_step()

x_0 <- -125
tmp <- with(data.frame(day = seq(min(polls_2008$day), max(polls_2008$day), .25)), 
            ksmooth(day, 1*I(day == x_0), kernel = "normal", x.points = day, bandwidth = span))
data.frame(x = tmp$x, w_0 = tmp$y) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_line()

tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) %>% 
     mutate(weight =  dnorm(dist, 0, span/2.54))%>%
     mutate(weight = weight/max(weight))
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="normal", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>%
     ggplot() +
     geom_smooth(aes(day, margin, group = center, weight = weight, frame = center), method = "lm", formula=y~1, se=FALSE) +
     geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(day, margin, size = weight, frame = center), show.legend = FALSE) +   
     scale_size(range = c(0, 3)) +
     geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
     ggtitle("x0 = ")
gganimate(p, interval= .1)

span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")

### Local Weighted Regression (loess)

span <- 21/diff(range(polls_2008$day))

tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(rank(dist) / n() <= span) %>%
     mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

tmp %>% 
     filter(center %in% c(-125, -55)) %>%
     ggplot(aes(day, margin)) +   
     scale_size(range = c(0, 3)) +
     geom_smooth(aes(group = center, weight = weight), 
                 method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(size = weight)) +
     facet_wrap(~center)

library(broom)
fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
loess_fit <- augment(fit)

p <- ggplot(tmp, aes(day, margin)) +
     scale_size(range = c(0, 3)) +
     geom_smooth(aes(group = center, frame = center, weight = weight), method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(size = weight, frame = center)) +
     geom_line(aes(x=day, y = .fitted, frame = day, cumulative = TRUE),
               data = loess_fit, color = "red") +
     ggtitle("x0 = ")
gganimate(p, interval= .1)

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth), color="red")

spans <- c(.66, 0.25, 0.15, 0.10)
fits <- data_frame(span = spans) %>% 
     group_by(span) %>% 
     do(broom::augment(loess(margin ~ day, degree=1, span = .$span, data=polls_2008)))
tmp <- fits %>%
     crossing(span = spans, center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(rank(dist) / n() <= span) %>%
     mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)
p <- ggplot(tmp, aes(day, margin)) +
     scale_size(range = c(0, 2)) +
     geom_smooth(aes(group = center, frame = center, weight = weight), method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 2, alpha = .5, color = "grey") +
     geom_line(aes(x=day, y = .fitted, frame = day, cumulative = TRUE),
               data = fits, color = "red") +
     geom_point(aes(size = weight, frame = center)) +
     facet_wrap(~span) +
     ggtitle("x0 = ")
gganimate(p, interval= .1)

tmp %>% ggplot(aes(day, margin)) +
     geom_point(size = 2, alpha = .5, color = "grey") +
     geom_line(aes(day, .fitted), data = fits, color = "red") +
     facet_wrap(~span)

data.frame(x = seq(min(polls_2008$day), max(polls_2008$day), length.out = 100)) %>%
     mutate(w_0 = (1 - (abs(x-x_0)/21)^3)^3*I(abs(x-x_0)<=21)) %>%
     ggplot(aes(x, w_0)) +
     geom_line()

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth_1), color="red", lty = 2) +
     geom_line(aes(day, smooth_2), color="orange", lty = 1) 

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth(color="red",  span = 0.15,
                 method.args = list(degree=1))
