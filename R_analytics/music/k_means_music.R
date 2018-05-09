#clear old data
rm(list=ls())

#load libraries
library(psy)
library(nFactors)
library(MASS)
library(psych)
library(GPArotation)
library(cluster)
library(ggplot2)
library(factoextra)
library(corrplot)
library(lattice)
library(NbClust)
library(readxl)

#get data
mydata = as.data.frame(read_excel(file.choose()))

#Prepare data
mydata.orig = mydata #save orig data copy

#view data
glimpse(mydata)

#check for missing data
apply(mydata, 2, function(x) any(is.na(x))) #no missing data
#mydata <- na.omit(mydata) # listwise deletion of missing
mydata[1] <- NULL #remove the first column

#set seed
set.seed(1)

#find optimal number of clusters
#using wss
fviz_nbclust(mydata, kmeans, method = "wss")

#using average silhouette
k.max <- 15
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(load, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(load))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)



#number of clusters
nclusters = 3

k <- kmeans(mydata, nclusters, nstart=25, iter.max=1000, 
            algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
            trace=FALSE)

#Visualize k-means
fviz_cluster(k, data = load, ellipse.type = "convex")+theme_minimal()
fviz_cluster(k, data = load, geom = "point",stand = FALSE, 
             ellipse.type = "norm")