# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Mall_Customers.csv")
X <- dataset[4:5]


# ----- 1/ K-Means Clustering
# Elbow method
set.seed(6)
wcss <- vector()
for (i in 1:10){wcss[i] <- sum(kmeans(X, i)$withinss)}
plot(1:10, wcss, type = "b", main = paste("Clusters of Clients"), 
     xlab = "Number of Clusters",
     ylab = "WCSS")
# 5 Clusters

# Model
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)
summary(kmeans)

# Visualisation
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of Clients"),
         xlab = "Annual Income",
         ylab = "Spending Score")


# ----- 2/ Hierarchical Clustering
# Dendrogram
dendrogram <- hclust(dist(X, method = "euclidean"),
                     method = "ward.D")
plot(dendrogram,
     main = paste("Dendrogram"),
     xlab = "Customers",
     ylab = "Euclidean Distances")
# 5 Clusters

# Model
hc <- hclust(dist(X, method = "euclidean"),
             method = "ward.D")
y_hc <- cutree(hc, k = 5)

# Visualisation
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clusters of Clients",
         xlab = "Annual Income",
         ylab = "Spending Score")