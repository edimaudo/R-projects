#remove old data
rm(list=ls())
#packages
packages <- c('factoextra','corrplot','NbClust', 'FactoMineR')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose())
#backup data
df.backup <- df

#remove columns
mydataset <- df[ ,c(-1,-2,-3,-7,-8,-11,-12,-13)]
#remove empty rows
#mydataset <- na.omit(mydataset)

#Apply MCA
mca <- MCA(mydataset, graph = FALSE)
print (mca)

# Visualizing Eigenvalues / Variances from the MCA 

# Extracting proportion of variances retained by the different dimensions (axes
eig.val <- get_eigenvalue(mca)
head(eig.val)

# Visualizing percentages of inertia explained by each MCA dimensions
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 2))

# Extracting the results for variable categories. 
# This function returns a list containing the coordinates, 
#the cos2 and the contribution of variable categories

var <- get_mca_var(mca)
print(var)

# Looking at the Coordinates, Cos2, and contribution each variable has to the principal components 

# Coordinates
head(var$coord)
# Cos2: quality on the factor map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# A plot showing the correlation of variables to the principle dimensions

fviz_mca_var(mca, choice = "mca.cor", 
             repel = FALSE,
             ggtheme = theme_minimal())

head(var$cos2, 4)

# Contributions of rows to dimension 1
fviz_contrib(mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(mca, choice = "var", axes = 1:2, top = 25)
# Biplot of individuals and variable categories
#fviz_mca_biplot(mca, repel = TRUE,ggtheme = theme_minimal())


#set.seed
set.seed(123)

#create kmeans data
km=data.frame(mca$ind$coord)

#find number of clusters
#using wss
fviz_nbclust(km, kmeans, method = "wss")

#using average silhouette
k.max <- 15
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(km, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(km))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

nclusters = 5 #based on number of clusters from information from above.  Can change as you need

#assign to clusters
k <- kmeans(km, nclusters, nstart=25, iter.max=1000, 
            algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
            trace=FALSE)

#Visualize k-means
#convex output
fviz_cluster(k, data = km, ellipse.type = "convex")+theme_minimal()
#ellipse output
fviz_cluster(k, data = km, geom = "point",stand = FALSE, 
             ellipse.type = "norm")

#groups.kmeans <- kmeans(km, centers = 3, nstart = 5)
#fviz_cluster(groups.kmeans, data = km, palette = "jco", repel= TRUE, main = "Kmeans", 
#             ggtheme = theme_classic())




