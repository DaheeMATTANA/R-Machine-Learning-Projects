# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Market_Basket_Optimisation.csv", header = FALSE)


# ----- 1/ Apriori
library(Matrix)
library(arules)
dataset <- read.transactions("Market_Basket_Optimisation.csv", sep = ",",
                             rm.duplicates = TRUE)

# Sparse Matrix
summary(dataset)
# On average, customers buy 4 items per each purchase

# Frequency Plot
itemFrequencyPlot(dataset, topN =10)

# Model
rules <- apriori(dataset, 
                 parameter = list(support = 0.003, confidence = 0.2))
# With this dataset, with default confidence of 0.8, we get no rule 
# Minimum support caculation (If we define, 3 transactions a week is the minimum) :
3*7/7500 

# Result Rules
inspect(sort(rules, by = "lift")[1:10])
# 7th, and 10th rules works in both ways
# Item Chocolate is in top 10 most purchased item, therefore, 
# rules including chocolate does not seem as a well associated rule
# Reduce the confidence (from 0.8 -> 0.2)

# Adjusting the model
rules <- apriori(dataset, 
                 parameter = list(support = 0.004, confidence = 0.2))
inspect(sort(rules, by = "lift")[1:10])


# ----- 2/ Eclat
# Model
rules <- eclat(dataset, parameter = list(support = 0.003, minlen = 2))
# minlen argument to define the minimum number of items in a set

# Result Rules
inspect(sort(rules, by = "support")[1:10])