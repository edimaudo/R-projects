#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies',
              'factoextra','NbClust')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = TRUE)

glimpse(df)

df_update <- df[,c(1,3,6,7,9,10,11,12,13,14,15,19)]

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_update, normalize))


#print(NbClust(df_cts, min.nc=2, max.nc=15, method="kmeans"))
#partition clustering
#using wss
fviz_nbclust(df_cts, kmeans, method = "wss")

#using average silhouette
k.max <- 20
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(df_cts, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_cts))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)