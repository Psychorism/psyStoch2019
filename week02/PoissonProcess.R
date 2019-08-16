library(ggplot2)
library(magrittr)

## https://www.r-bloggers.com/%F0%9F%93%88-simulating-poisson-process-part-1/

sim_pp1 <- function(t, rate) { 
  
  path <- matrix(0, nrow = 1, ncol = 2)
  
  jumps_time <- rexp(1, rate)
  
  while(jumps_time[length(jumps_time)] < t) {
    
    jump <- matrix(c(jumps_time[length(jumps_time)], path[nrow(path), 2],
                     jumps_time[length(jumps_time)], path[nrow(path), 2]  + 1),
                   nrow = 2, ncol = 2, byrow = TRUE)
    
    path <- rbind(path, jump)
    
    jumps_time <- c(jumps_time,
                    jumps_time[length(jumps_time)] + rexp(1, rate))
  }
  
  path <- rbind(path,
                c(t, path[nrow(path), 2]))
  
  list(path, jumps_time)
}

sim_pp2 <- function(t, rate) {
  
  path <- matrix(0, nrow = 1, ncol = 2)
  
  jumps_number <- rpois(1, lambda = rate * t)
  jumps_time <- runif(n = jumps_number, min = 0, max = t) %>% sort()
  
  for(j in seq_along(jumps_time)) {
    jump <- matrix(c(jumps_time[j], path[nrow(path), 2],
                     jumps_time[j], path[nrow(path), 2]  + 1),
                   nrow = 2, ncol = 2, byrow = TRUE)
    path <- rbind(path, jump)
  }
  
  path <- rbind(path,
                c(t, path[nrow(path), 2]))
  
  list(path, jumps_time)
  
}


path1 <- sim_pp1(1000, 1)
mean(diff(path1[[2]])); var(diff(path1[[2]]))


data.frame(it = diff(path1[[2]])) %>%
  ggplot() +
  geom_histogram(aes(it, y = ..density..)) +
  stat_function(fun = dexp) +
  theme_bw() + 
  theme(text = element_text(size = 24))

path2 <- sim_pp2(1000, 1)
mean(diff(path2[[2]])); var(diff(path2[[2]]))

data.frame(it = diff(path2[[2]])) %>%
  ggplot() +
  geom_histogram(aes(it, y = ..density..)) +
  stat_function(fun = dexp) +
  theme_bw() + 
  theme(text = element_text(size = 24))


t <- 10
n <- 2000
rate = 1

paths1 <- replicate(n = n, expr = sim_pp1(t, rate), simplify = FALSE)
means1 <- sapply(1:n,
                 function(x) {
                   pathes <- paths1[1:x]
                   mean(sapply(pathes, function(y) y[[1]][nrow(y[[1]]), 2]))
                 })

paths2 <- replicate(n = n, expr = sim_pp2(t, rate), simplify = FALSE)
means2 <- sapply(1:n,
                 function(x) {
                   pathes <- paths2[1:x]
                   mean(sapply(pathes, function(y) y[[1]][nrow(y[[1]]), 2]))
                 })

rbind(data.frame(n = 1:n, mean = means1, method = "1"),
      data.frame(n = 1:n, mean = means2, method = "2")) %>%
  ggplot() +
  geom_line(aes(x = n, y = mean, color = method)) + 
  geom_hline(yintercept = rate * t) +
  theme_bw() + 
  theme(text = element_text(size = 24))

paths1 <- replicate(n = 2000, expr = sim_pp1(10, 1), simplify = FALSE)
probs1 <- sapply(1:2000,
                 function(x) {
                   pathes <- paths1[1:x]
                   mean(sapply(pathes, function(y) y[[1]][nrow(y[[1]]), 2]) <= 10)
                 })

paths2 <- replicate(n = 2000, expr = sim_pp2(10, 1), simplify = FALSE)
probs2 <- sapply(1:2000,
                 function(x) {
                   pathes <- paths2[1:x]
                   mean(sapply(pathes, function(y) y[[1]][nrow(y[[1]]), 2]) <= 10)
                 })

rbind(data.frame(n = 1:n, prob = probs1, method = "1"),
      data.frame(n = 1:n, prob = probs2, method = "2")) %>%
  ggplot() +
  geom_line(aes(x = n, y = prob, color = method)) + 
  geom_hline(yintercept = ppois(q = 10, lambda = t * rate)) +
  theme_bw() + 
  theme(text = element_text(size = 24))