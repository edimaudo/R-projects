#clustering
#remove old data
rm(list=ls())

#load libraries
library(tidyverse)
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

df <- read_csv(file.choose())

#backup
df.orig = df #save orig data copy

#correlation
corMat = cor(df[,3:8])
corrplot(corMat, method = 'number')
corrplot(corMat, order = "hclust", tl.col='black', tl.cex=.75) 

#kmeans clustering
#find number of clusters needed
df_main <- df[,3:8]
df_main <- na.omit(df_main)
k <- NbClust(df_main, distance = "euclidean",
              min.nc = 2, max.nc = 10, 
              method = "complete", index ="all")

print(NbClust(df_main, min.nc=2, max.nc=15, method="kmeans"))
#partition clustering
#using wss
fviz_nbclust(df_main, kmeans, method = "wss")

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
kfit <- kmeans(df_main, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = df_main, ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = df_main, geom = "point",stand = FALSE, 
             ellipse.type = "norm")