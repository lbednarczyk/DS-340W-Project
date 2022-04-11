
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)

# Define the control using a random forest selection function
featurecontrol <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
x <- data %>%
  select(-target) %>%
  as.data.frame()

# Target variable
y <- data$target

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:13),
                   metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                   rfeControl = featurecontrol)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "Rsquared") + theme_bw()
