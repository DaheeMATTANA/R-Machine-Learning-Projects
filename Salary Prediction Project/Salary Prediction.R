# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Salary_Data.csv")


# ----- 1/ Linear Regression
# Set Split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Model
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
summary(regressor)

# Prediction
y_pred <- predict(regressor, newdata = test_set)

# Visualisation
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Salary vs Experience (Test Set)") +
  xlab("Years of Experience") +
  ylab("Salary")


# ----- Data
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[2:3]

# ----- 2/ Polynomial Regression
# Linear model
lin_reg <- lm(formula = Salary ~ ., data = dataset)
summary(lin_reg)

# Add polynomial features
dataset$Level2 <- dataset$Level ^ 2
dataset$Level3 <- dataset$Level ^ 3
dataset$Level4 <- dataset$Level ^ 4
poly_reg <- lm(formula = Salary ~ ., data = dataset)
summary(poly_reg)

# Single observation prediction with linear regression
y_pred <- predict(lin_reg, data.frame(Level = 6.5))

# Single observation prediction with polynomial regression
y_pred <- predict(poly_reg, data.frame(Level = 6.5, Level2 = 6.5^2,
                                       Level3 = 6.5^3, Level4 = 6.5^4))

# Visualisation
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = "blue") +
  ggtitle("Truth or Bluff (Polynomial Regression)") +
  xlab("Level") +
  ylab("Salary")


# ----- 3/ Support Vector Regression
# Model
library(e1071)
regressor <- svm(formula = Salary ~ ., data = dataset,
                 type = 'eps-regression')

# Single observation prediction
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualisation
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Truth or Bluff (SVR)") +
  xlab("Level") +
  ylab("Salary")


# ----- 4/ Decision Tree Regression
# Model
library(rpart)
regressor <- rpart(formula = Salary ~ ., data = dataset,
                   control = rpart.control(minsplit = 1))

# Single observation prediction
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualisation
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.001)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Decision Tree Regression") +
  xlab("Level") +
  ylab("Salary")


# ----- 5/ Random Forest Regression
# Model
library(randomForest)
set.seed(1234)
regressor <- randomForest(x = dataset[1], 
                          y = dataset$Salary,
                          ntree = 500)

# Single observation prediction
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualisation
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.001)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Random Forest Regression") +
  xlab("Level") +
  ylab("Salary")