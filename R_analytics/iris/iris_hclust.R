
library(hclust)


mydata <- iris
summary(mydata)


clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, mydata$Species)
clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)