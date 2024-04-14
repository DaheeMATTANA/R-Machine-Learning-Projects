# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Wine_profile.csv")


# ----- Preprocessing
# Set split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] <- scale(training_set[-14])
test_set[-14] <- scale(test_set[-14])


# ----- 1/ Dimensionality Reduction - PCA
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
pca <- preProcess(x = training_set[-14], method = "pca", pcaComp = 2)
training_set_pca <- predict(pca, training_set)
# Put the dependent variable at the last column
training_set_pca <- training_set_pca[c(2, 3, 1)]

test_set_pca <- predict(pca, test_set)
test_set_pca <- test_set_pca[c(2, 3, 1)]

# Support Vector Machine Model
classifier <- svm(formula = Customer_Segment ~ .,
                  data = training_set_pca,
                  type = "C-classification",
                  kernel = "linear")

# Prediction
y_pred <- predict(classifier, newdata = test_set_pca[-3])

# Evaluation
cm <- table(test_set_pca[, 3], y_pred)
cm
# 83% accuracy

# Visualisation
library(ElemStatLearn)
set <- test_set_pca
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("PC1", "PC2")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "SVM (Test Set)",
     xlab = "PC1", ylab = "PC2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, "dodgerblue", ifelse(y_grid == 1, "springgreen", "salmon")))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "dodgerblue3", ifelse(set[, 3] == 1, "springgreen3", "salmon3")))


# ----- 2/ Dimensionality Reduction - LDA
library(MASS)
lda <- lda(formula = Customer_Segment ~ .,
           data = training_set)
training_set_lda <- as.data.frame(predict(lda, training_set))
training_set_lda <- training_set_lda[c(5, 6, 1)]
test_set_lda <- as.data.frame(predict(lda, test_set))
test_set_lda <- test_set_lda[c(5, 6, 1)]

# Support Vector Machine Model
classifier <- svm(formula = class ~ .,
                  data = training_set_lda,
                  type = "C-classification",
                  kernel = "linear")

# Prediction
y_pred <- predict(classifier, newdata = test_set_lda[-3])

# Evaluation
cm <- table(test_set_lda[, 3], y_pred)
cm
# 1 incorrect prediction

# Visualisation
set <- test_set_lda
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("x.LD1", "x.LD2")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = "SVM (Test Set)",
     xlab = "LD1", ylab = "LD2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, "dodgerblue", ifelse(y_grid == 1, "springgreen", "salmon")))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "dodgerblue3", ifelse(set[, 3] == 1, "springgreen3", "salmon3")))