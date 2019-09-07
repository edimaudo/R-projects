#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','NbClust','nFactors','scales',
              'factoextra','cluster','psy')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = TRUE)

glimpse(df)

df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#drop first column
df[1] <- NULL

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df <- as.data.frame(lapply(df, normalize))

#2. Determine number of clusters
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k <- NbClust(df, distance = "euclidean",
             min.nc = 2, max.nc = 10, 
             method = "complete", index ="all")
print(NbClust(df, min.nc=2, max.nc=15, method="kmeans"))

#partition clustering using wss
fviz_nbclust(df, kmeans, method = "wss")

k1 = 2

# Generate K-mean clustering
kfit <- kmeans(df, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = df, ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = df, geom = "point",stand = FALSE, 
             ellipse.type = "norm")