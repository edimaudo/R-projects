library("readxl")
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

#remove all data
rm(list=ls())

white_wine_data <- read_excel(file.choose())

#summary
summary(white_wine_data)

#data prep
white_wine_data.orig = white_wine_data #save orig data copy
white_wine_data <- na.omit(white_wine_data) # listwise deletion of missing

#correlation between columns
corMat = cor(white_wine_data)
corrplot(cor(white_wine_data), order = "hclust", tl.col='black', tl.cex=.75) 

#k-means clustering
set.seed(1)

#find optimal number of clusters


k <- NbClust(white_wine_data, distance = "euclidean",
              min.nc = 2, max.nc = 10, 
              method = "complete", index ="all")
print(NbClust(white_wine_data, min.nc=2, max.nc=15, method="kmeans"))
#partition clustering

#using wss
fviz_nbclust(white_wine_data, kmeans, method = "wss")

#using average silhouette
k.max <- 15
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(mydata, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(mydata))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

# Use optimal no. of clusters in k-means #
k1=3 # compare with multiple k values

# Generate K-mean clustering
kfit <- kmeans(white_wine_data, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = white_wine_data, ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = white_wine_data, geom = "point",stand = FALSE, 
             ellipse.type = "norm")


white_wine_data_mean <- aggregate(white_wine_data,by=list(kfit$cluster),FUN=mean)
white_wine_data_median <- t(aggregate(white_wine_data,by=list(kfit$cluster),FUN=median))