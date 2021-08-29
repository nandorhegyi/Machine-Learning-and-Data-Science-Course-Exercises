dataset=read.csv('Mall_Customers.csv')
dataset=dataset[, 4:5]

#using the dendrogram to find the optimal number of clusters
dendrogram= hclust(dist(dataset, method='euclidean'), method='ward.D')
plot(dendrogram,
     main=paste('Dendrogram'),
     xlab='Customers',
     ylab='Euclidean distances')

#fitting hierarchical clustering to the dataset
hc= hclust(dist(dataset, method='euclidean'), method='ward.D')
y_hc=cutree(hc,5)

#visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')