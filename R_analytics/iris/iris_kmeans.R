library(cluster)
library(ggplot2)
library(factoextra)
library(corrplot)
library(psy)
library(lattice)
library(nFactors)
library(RColorBrewer)
library(scales)
library(NbClust)

mydata <- iris
summary(mydata)

ggplot(mydata,aes(mydata$Petal.Length,mydata$Petal.Width, color = Species)) + 
  geom_point() + theme_minimal()

set.seed(20)
irisCluster <- kmeans(mydata[, 3:4], 3, nstart = 20)
print(irisCluster)

k <- NbClust(mydata[, 3:4], distance = "euclidean",
             min.nc = 2, max.nc = 10, 
             method = "complete", index ="all")
print(NbClust(mydata, min.nc=2, max.nc=15, method="kmeans"))

#partition clustering using wss
fviz_nbclust(mydata[, 3:4], kmeans, method = "wss")

k1 = 3

# Generate K-mean clustering
kfit <- kmeans(mydata[, 3:4], k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = mydata[, 3:4], ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = mydata[, 3:4], geom = "point",stand = FALSE, 
             ellipse.type = "norm")