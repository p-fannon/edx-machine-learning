set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# Top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# The school's students take tests
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Top 10 by score
schools %>% top_n(10, score) %>% arrange(desc(score))

# Answer
top_10_schools <- schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# Median size of all schools
median(schools$size)
# Median schools size of the top 10
median(top_10_schools$size)

# Answer
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

# Median school size of 10 worst schools based on score
schools %>% top_n(-10, score) %>% .$size %>% median()

plot(schools$score, schools$size)

# Answer
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

# Regulatization
overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Which alpha from 10 to 250 gives lowest RMSE
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Rank the schools based on the average obtained with the best  𝛼
# best_alpha <- 135
best_alpha <- alphas[which.min(rmse)] 
alpha_score_reg <- sapply(scores, function(x) overall + sum(x-overall)/(length(x)+best_alpha))
schools %>% mutate(alpha_score_reg = alpha_score_reg) %>%
  top_n(10, alpha_score_reg) %>% arrange(desc(alpha_score_reg))

# Best alpha when not subtracting overall mean before shrinking
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
