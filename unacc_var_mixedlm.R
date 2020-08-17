# Import Libraries

library(tidyverse)
library(raster)

library(lme4)
library(lmerTest)

# Additional variation

set.seed(8376)

level.num <- 50
sublevels <- 1:level.num

study.n <- 100

confint.results <- data.frame(study.id = 1:study.n)
lower.results <- rep(0, study.n)
upper.results <- rep(0, study.n)
estimates <- rep(0, study.n)

n.vector <- c(1000, 2500, 5000)
for (n in n.vector) {
  
  for (i in 1:study.n) {
    subintercepts <- runif(level.num, 5, 15)
    subslopes <- rnorm(level.num, mean = 0, sd = 5)
    
    sl <- sample(sublevels, size = n, replace = TRUE)
    group <- sample(c(0,1), size = n, replace = TRUE)
    y <- sapply(1:n, function(i) {
      return(subslopes[sl[i]]*group[i] + subintercepts[sl[i]])
    }) + rnorm(n, mean = 0, sd = 1)
    
    data <- data.frame(group, sl, y)
    
    group1 <- data %>%
      filter(group == 0)
    group2 <- data %>%
      filter(group == 1)
    
    result <- t.test(group2$y, group1$y)
    lower.results[i] <- result$conf.int[1]
    upper.results[i] <- result$conf.int[2]
    estimates[i] <- result$estimate[["mean of x"]] - result$estimate[["mean of y"]]
  }
  
  confint.results$lower <- lower.results
  confint.results$upper <- upper.results
  confint.results$estimate <- estimates
  confint.results <- confint.results %>%
    mutate(contain_param = ifelse(lower <= 0 & upper >= 0, TRUE, FALSE))
  
  meta_avg_effect <- mean(confint.results$estimate)
  
  g <- confint.results %>%
    ggplot(aes(x=estimate, y = reorder(study.id, estimate))) +
    geom_point(size = 1) +
    geom_errorbar(aes(xmin=lower, xmax=upper, col=contain_param), width=.1) +
    geom_vline(xintercept = 0, col = "red") +
    ggtitle(paste0("n = ", n)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(g)
}

# No slope variance

level.num <- 50
sublevels <- 1:level.num

study.n <- 100

confint.results <- data.frame(study.id = 1:study.n)
lower.results <- rep(0, study.n)
upper.results <- rep(0, study.n)
estimates <- rep(0, study.n)

n <- 2500

for (i in 1:study.n) {
  subintercepts <- runif(level.num, 5, 15)
  subslopes <- rnorm(level.num, mean = 0, sd = 5)
  
  sl <- sample(sublevels, size = n, replace = TRUE)
  group <- sample(c(0,1), size = n, replace = TRUE)
  y <- sapply(1:n, function(i) {
    return(subintercepts[sl[i]])
  }) + rnorm(n, mean = 0, sd = 1)
  
  data <- data.frame(group, sl, y)
  
  group1 <- data %>%
    filter(group == 0)
  group2 <- data %>%
    filter(group == 1)
  
  result <- t.test(group2$y, group1$y)
  lower.results[i] <- result$conf.int[1]
  upper.results[i] <- result$conf.int[2]
  estimates[i] <- result$estimate[["mean of x"]] - result$estimate[["mean of y"]]
}

confint.results$lower <- lower.results
confint.results$upper <- upper.results
confint.results$estimate <- estimates
confint.results <- confint.results %>%
  mutate(contain_param = ifelse(lower <= 0 & upper >= 0, TRUE, FALSE))

meta_avg_effect <- mean(confint.results$estimate)

g <- confint.results %>%
  ggplot(aes(x=estimate, y = reorder(study.id, estimate))) +
  geom_point(size = 1) +
  geom_errorbar(aes(xmin=lower, xmax=upper, col=contain_param), width=.1) +
  geom_vline(xintercept = 0, col = "red") +
  ggtitle(paste0("n = ", n)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

print(g)


# Mixed effect model

level.num <- 50
sublevels <- 1:level.num

study.n <- 100

confint.results <- data.frame(study.id = 1:study.n)
lower.results <- rep(0, study.n)
upper.results <- rep(0, study.n)
estimates <- rep(0, study.n)

n <- 2500

for (i in 1:study.n) {
  subintercepts <- runif(level.num, 5, 15)
  subslopes <- rnorm(level.num, mean = 0, sd = 5)
  
  sl <- sample(sublevels, size = n, replace = TRUE)
  group <- sample(c(0,1), size = n, replace = TRUE)
  y <- sapply(1:n, function(i) {
    return(subslopes[sl[i]]*group[i] + subintercepts[sl[i]])
  }) + rnorm(n, mean = 0, sd = 1)
  
  data <- data.frame(group, sl, y)
  
  mixed.lmer <- lmer(y ~ group + (1 + group|sl), data = data)
  test.result <- contest1D(mixed.lmer, c(0,1), confint = TRUE)
  
  lower.results[i] <- test.result$lower
  upper.results[i] <- test.result$upper
  estimates[i] <- test.result$Estimate
}

confint.results$lower <- lower.results
confint.results$upper <- upper.results
confint.results$estimate <- estimates
confint.results <- confint.results %>%
  mutate(contain_param = ifelse(lower <= 0 & upper >= 0, TRUE, FALSE))

meta_avg_effect <- mean(confint.results$estimate)

g <- confint.results %>%
  ggplot(aes(x=estimate, y = reorder(study.id, estimate))) +
  geom_point(size = 1) +
  geom_errorbar(aes(xmin=lower, xmax=upper, col=contain_param), width=.1) +
  geom_vline(xintercept = 0, col = "red") +
  ggtitle(paste0("n = ", n)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

print(g)