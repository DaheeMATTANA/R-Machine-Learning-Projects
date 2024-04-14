# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[, 3:5]


# ----- 1/ Logistic Regression
# Set Split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

# Model
classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training_set)

# Prediction
# STEP 1, get the probability of the independent variable
prob_pred <- predict(classifier, type = "response", newdata = test_set[-3])
# STEP 2, convert into 1, 0 values
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 83% accuracy

# Visualisation
library(ElemStatLearn)
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "Logistic Regression (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 2/ K-Nearest Neighbors
# Encoding factors
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

# Set Split
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# Model & Prediction
library(class)
y_pred <- knn(train = training_set[, -3],
              test = test_set[, -3],
              cl = training_set[, 3],
              k = 5)

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 89% accuracy

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- knn(train = training_set[, -3],
              test = grid_set,
              cl = training_set[, 3],
              k = 5)
plot(set[, -3],
     main = "K-NN (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 3/ Support Vector Machine
# Model
library(e1071)
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "linear")

# Prediction
y_pred <- predict(classifier, newdata = test_set[-3])

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 80% accuracy

# K Fold Cross Validation
library(caret)
folds <- createFolds(training_set$Purchased, k = 10)
cv <- lapply(folds, function(x){
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- svm(formula = Purchased ~ .,
                    data = training_fold,
                    type = "C-classification",
                    kernel = "radial")
  y_pred <- predict(classifier, newdata = test_fold[-3])
  cm <- table(test_fold[, 3], y_pred)
  accuracy <- (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[1, 2] + cm[2, 1] + cm[2, 2])
  return(accuracy)
})
cv
# Calculate the mean
accuracy <- mean(as.numeric(cv))
accuracy
# 91.6% with Cross Val Score

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "SVM (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 4/ Kernel SVM
# Model
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "radial")

# Prediction
y_pred <- predict(classifier, newdata = test_set[-3])

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 90% accuracy

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Kernel SVM (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))

# ----- 4-1/ Kernel SVM with Grid Search
classifier <- train(form = Purchased ~ .,
                    data = training_set,
                    method = "svmRadial")
classifier
# Get the hyperparameter
classifier$bestTune
# sigma = 1.34457, C = 1
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "radial",
                  sigma = 1.34457,
                  C = 1)

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Kernel SVM (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 5/ Decision Tree Classification
# Model
library(rpart)
classifier <- rpart(formula = Purchased ~ .,
                    data = training_set)

# Prediction
y_pred <- predict(classifier, newdata = test_set[-3], type = "class")

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 83% accuracy

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = "Decision Tree (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 6/ Naive Bayes
# Model
classifier <- naiveBayes(x = training_set[-3],
                         y = training_set$Purchased)

# Prediction
y_pred <- predict(classifier, newdata = test_set[-3])

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 86% accuracy

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Naive Bayes (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))


# ----- 7/ Random Forest Classification
# Model
library(randomForest)
classifier <- randomForest(x = training_set[-3],
                           y = training_set$Purchased,
                           ntree = 10)

# Prediction
y_pred <- predict(classifier, newdata = test_set[-3])

# Evaluation
cm <- table(test_set[, 3], y_pred)
cm
# ----- 86% accuracy

# Visualisation
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "Random Forest (Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))
# This Model has been OVERFITTED