# Credit : superdatascience.com


# ----- Data
dataset <- read.delim("Restaurant_Reviews.tsv", quote = "", 
                      stringsAsFactors = FALSE)

dataset_original <- read.delim("Restaurant_Reviews.tsv", quote = "", 
                               stringsAsFactors = FALSE)

# ----- Preprocessing
library(NLP)
library(tm)
library(SnowballC)

# Creating corpus
corpus <- VCorpus(VectorSource(dataset$Review))
as.character(corpus[[1]])     # index of the first review

# Lower case conversion
corpus <- tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])
as.character(corpus[[841]])

# Getting rid of numbers
corpus <- tm_map(corpus, removeNumbers)
as.character(corpus[[841]])

# Getting rid of punctuations
corpus <- tm_map(corpus, removePunctuation)
as.character(corpus[[1]])

# Removing all the non-relevant words (such as "this", "the")
corpus <- tm_map(corpus, removeWords, stopwords())
as.character(corpus[[1]])

# Stemming
corpus <- tm_map(corpus, stemDocument)
as.character(corpus[[1]])

# Removing whitespaces
corpus <- tm_map(corpus, stripWhitespace)
as.character(corpus[[841]])


# ----- Bag of Words
# Sparse Matrix of features (rows of all reviews, columns of all words)
dtm <- DocumentTermMatrix(corpus)
dtm
# 1577 words

# Reduce more sparsity by filtering (only consider most frequent words)
dtm <- removeSparseTerms(dtm, 0.999)  
# parameters : Proportion of the non-frequent words that we want to remove, Keep 99% of the most frequent words
# 691 words kept
dtm


# ----- Random Forest
# dtm is a matrix, convert into dataset
dataset <- as.data.frame(as.matrix(dtm))
# Dependent variable is missing
dataset$Liked <- dataset_original$Liked

# Encoding the Target Feature As Factor
dataset$Liked <- factor(dataset$Liked, levels = c(0, 1))

# Set Split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Liked, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Model
library(randomForest)
classifier <- randomForest(x = training_set[-692],
                           y = training_set$Liked,
                           ntree = 10)

# Prediction
y_pred <- predict(classifier, newdata = test_set[-692])

# Evaluation
cm = table(test_set[, 692], y_pred)
cm
(82+77)/200
# 79.5% Accuracy