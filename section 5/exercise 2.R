# My attempt
library(rpart)
library(caret)
data("tissue_gene_expression")
set.seed(1991, sample.kind = "Rounding")
fit <- train(y ~ ., method="rpart", data= tissue_gene_expression$y, tuneGrid = data.frame(k = seq(0, 0.1, 0.01)))
plot(fit)

# Answer
library(caret)
library(rpart)          
library(dslabs)
# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)

fit_rpart <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit_rpart)
confusionMatrix(fit)
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
