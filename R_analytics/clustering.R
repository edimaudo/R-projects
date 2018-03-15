#segmentation

##packages
#install.packages("factoextra")
#install.packages("mclust")
#install.packages("corrplot")
#install.packages("NbClust")
#install.packages("psych")

#load libraries
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

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

#Data prep.
mydata.orig = mydata #save orig data copy
mydata <- na.omit(mydata) # listwise deletion of missing

##take a look at the data
#View(mydata)

#head(mydata)

#Check if data is metric
summary(mydata)
mydata <- scale(mydata) # standardize variables

#correlation between columns
corMat = cor(mydata)
corrplot(cor(mydata), order = "hclust", tl.col='black', tl.cex=.75) 

#data distance
#res.dist <- get_dist(mydata, stand = TRUE, method = "pearson")
#fviz_dist(res.dist, 
#          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#data with all attributes
set.seed(1)

#clustering
#find number of clusters needed

# k <- NbClust(mydata, distance = "euclidean",
#              min.nc = 2, max.nc = 10, 
#              method = "complete", index ="all")

# #hierarchal clustering
# # 2. Compute dissimilarity matrix
# d <- dist(mydata, method = "euclidean")
# # Hierarchical clustering using Ward's method
# res.hc <- hclust(d, method = "ward.D2" )
# # Cut tree into 4 groups
# grp <- cutree(res.hc, k = 4)
# # Visualize
# plot(res.hc, cex = 0.6) # plot tree
# rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle


print(NbClust(mydata, min.nc=2, max.nc=15, method="kmeans"))
#partition clustering
#using wss
fviz_nbclust(mydata, kmeans, method = "wss")

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
kfit <- kmeans(mydata, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = mydata, ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = mydata, geom = "point",stand = FALSE, 
             ellipse.type = "norm")

#cluster validation
#get_clust_tendency(mydata, n = 50,
#                   gradient = list(low = "steelblue",  high = "white"))

#assign clusters to orginal 
mydataAttributes <- data.frame(mydata_attr, kfit$cluster)

table(mydata_attr, kfit$cluster)

# plot the clustered points along sepal length and width
plot(kmean_iris[c("Sepal.Length", "Sepal.Width")],
     col=clusters$cluster,pch = c(15,16,17)[as.numeric(clusters$cluster)])
points(clusters$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3,
       pch=8, cex=2)


# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
# library(cluster)
# clusplot(mydata_attr, kfit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# 
# #Perceptual map
# pc.cr <-princomp(mydata_attr, cor = TRUE) #scale data
# summary(pc.cr)
# biplot(pc.cr)

#summary statistics 
# summary(subset(mydataAttributes, kfit.cluster==1))
# cl1rowslength = sum(kfit$cluster==1)
# print(cl1rowslength)
# summary(subset(mydataAttributes, kfit.cluster==2))
# cl2rowslength = sum(kfit$cluster==2)
# print(cl2rowslength)
# summary(subset(mydataAttributes, kfit.cluster==3))
# cl3rowslength = sum(kfit$cluster==3)
# print(cl3rowslength)
