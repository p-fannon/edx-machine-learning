install.packages("dplyr")
library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

sepal_length_seq <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
x <- iris$Sepal.Length
sepal_length_accuracy <- map_dbl(sepal_length_seq, function(x){
  sepal_length_y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(sepal_length_y_hat == train$Species)
})
max(sepal_length_accuracy)

x <- iris$Sepal.Width
sepal_width_seq <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
sepal_width_accuracy <- map_dbl(sepal_width_seq, function(x){
  sepal_width_y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(sepal_width_y_hat == train$Species)
})
max(sepal_width_accuracy)

x <- iris$Petal.Length
petal_length_seq <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
petal_length_accuracy <- map_dbl(petal_length_seq, function(x){
  petal_length_y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(petal_length_y_hat == train$Species)
})
max(petal_length_accuracy)

x <- iris$Petal.Width
petal_width_seq <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
petal_width_accuracy <- map_dbl(petal_width_seq, function(x){
  petal_width_y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(petal_width_y_hat == train$Species)
})
max(petal_width_accuracy)

# ANSWER, finds Petal.Length to be most accurate
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

# Overall accuracy of test data with train$Petal.Length feature
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# Predicts using petal length and width
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
