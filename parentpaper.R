library(data.table)
library(tidyverse)
library(dplyr)
library(matrixStats)
library(gam)
library(evtree)
library(caret)
library(knitr)
library(rpart)
library(naivebayes)
library(stats)

set.seed(8)
data <- fread('./volume/data/raw/heart.csv')

x <- data[,-14]
y <- data$target

set.seed(10, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
test_y <- y[test_index]
train_x <- x[-test_index,]
train_y <- y[-test_index]

#checking to see if train/test are proportioned properly
options(digits = 3)
round(mean(test_y == 0), 3)
round(mean(train_y == 0), 3)

#k-fold cross validation with k = 10
control <- trainControl(method = "cv", number = 10, p = 0.9)

#----------CREATING MODELS-------------

##RANDOM FOREST
set.seed(8, sample.kind = "Rounding")
tuning <- data.frame(mtry = seq(1, 25, 2))
train_rf <- train(train_x, as.factor(train_y), method = "rf",
                  tuneGrid = tuning,
                  trControl = control,
                  importance = TRUE)

#creating graph of hyperparameter tuning results
ggplot(train_rf, highlight = TRUE) + ggtitle("Random Forest")

#find best tuning outcome
train_rf$bestTune

rf_preds <- predict(train_rf, test_x)
Random_Forest <- confusionMatrix(rf_preds, as.factor(test_y), positive = "1")

#variable importance
varImp(train_rf)

#DECISION TREE
train_dt <- train(train_x, as.factor(train_y), method = 'rpart',
                  trControl = control)
dt_preds <- predict(train_dt, test_x)
decision_tree <- confusionMatrix(dt_preds, as.factor(test_y), positive = "1")

##LOGISTIC REGRESSION
train_glm <- train(train_x, as.factor(train_y), method = "glm",
                   family = "binomial",
                   trControl = control)
glm_preds <- predict(train_glm, test_x)
logistic_regression <- confusionMatrix(glm_preds, as.factor(test_y), positive = "1")

#NAIVE BAYES
train_nb <- train(train_x, as.factor(train_y), method = "naive_bayes",
                  trControl = control)
nb_preds <- predict(train_nb, test_x)
naive_bayes <- confusionMatrix(nb_preds, as.factor(test_y), positive = "1")


#print confusion matrix and statistics for each model
Random_Forest
decision_tree
logistic_regression
naive_bayes