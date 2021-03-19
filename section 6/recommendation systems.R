library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

fit <- lm(rating ~ as.factor(movieId), data = movielens)

mu <- mean(train_set$rating) 
# Next line runs very slowly
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
#> `summarise()` ungrouping output (override with `.groups` argument)
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)
#> [1] 0.989

train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
#> `summarise()` ungrouping output (override with `.groups` argument)

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#> `summarise()` ungrouping output (override with `.groups` argument)

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)