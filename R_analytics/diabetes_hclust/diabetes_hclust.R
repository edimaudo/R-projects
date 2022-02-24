
library(hclust)

mydata <- read.csv("Clustering_diabetesdata.csv")
summary(mydata)


clusters <- hclust(dist(mydata))
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, mydata$Species)
clusters <- hclust(dist(mydata), method = 'average')
plot(clusters)