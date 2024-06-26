# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Churn_Modelling.csv")
dataset <- dataset[4:14]


# ----- Preprocessing
# Encoding factors & Setting them as NUMERIC
dataset$Geography <- as.numeric(factor(dataset$Geography,
                            levels = c("France", "Spain", "Germany"),
                            labels = c(1, 2, 3)))
dataset$Gender <- as.numeric(factor(dataset$Gender,
                         levels = c("Female", "Male"),
                         labels = c(1, 2)))

# Set split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set[-11] <- scale(training_set[-11])
test_set[-11] <- scale(test_set[-11])

# ----- 1/ Artificial Neural Network
# Model
library(h2o)
h2o.init(nthreads = -1)    # connect to the system
classifier <- h2o.deeplearning(y = "Exited",
                               training_frame = as.h2o(training_set),
                               activation = "Rectifier",
                               hidden = c(6, 6),
                               epochs = 100,
                               train_samples_per_iteration = -2)
# hidden = c(number of neurons in the 1st hidden layer, number of neurons in the 2nd hidden layer)

# Prediction
prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred <- (prob_pred > 0.5)     # Boolean value will be accepted at the confusion matrix stage
# Convert the h2o object back into a vector
y_pred <- as.vector(y_pred)

# Evaluation
cm <- table(test_set[, 11], y_pred)
cm
(1513+218)/(1513+80+189+218)
# 86.5% accuracy
# Recall : 88.9
1513/(1513+189)
# Precision : 95
1513/(1513+80)

# Disconnect H2O
h2o.shutdown()


# ----- 2/ XGBoost
# Model
library(xgboost)
classifier <- xgboost(data = as.matrix(training_set[-11]),
                      label = training_set$Exited,
                      nrounds = 10)

# Evluation with k fold cross validation
library(caret)
folds <- createFolds(training_set$Exited, k = 10)
cv <- lapply(folds, function(x){
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- xgboost(data = as.matrix(training_set[-11]),
                        label = training_set$Exited,
                        nrounds = 10)
  y_pred <- predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred <- (y_pred >= 0.5)
  cm <- table(test_fold[, 11], y_pred)
  accuracy <- (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1])
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))
accuracy
# 88%