
packages <- c('ggplot2','tidyverse','shiny','shinydashboard',
              'scales','dplyr','lubridate','reshape2',
              'corrplot','psy','lattice','NbClust','factorextra')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data
df <- read.csv("Clustering_diabetesdata.csv") 

set.seed(20)
k <- NbClust(df, distance = "euclidean",
             min.nc = 2, max.nc = 10, 
             method = "complete", index ="all")
print(NbClust(df, min.nc=2, max.nc=15, method="kmeans"))
#partition clustering using silhuette
fviz_nbclust(df, kmeans, method = "silhouette")

k1 = 2

# Generate K-mean clustering
kfit <- kmeans(df, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = df, ellipse.type = "convex")+theme_minimal()
fviz_cluster(kfit, data = df, geom = "point",stand = FALSE, 
             ellipse.type = "norm")