#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','readxl',
              'cluster','factoextra','psy','lattice','nFactors','scales','NbClust')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df_train <- read.table(file.choose(),sep = ",",header = TRUE)
df_test <- read.table(file.choose(),sep = ",",header = TRUE)

#combine train and test
df_train <- df_train[,c(1:12)]
df <- rbind(df_train,df_test)

#get summary
summary(df)

#remove missing data
#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)
df <- na.omit(df)

#drop unncessary columns
#delete first column
df[1] <- NULL

#recode categorical data
df_cat <- df[,c(1,2,3,4,5,11)]
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts <- df[,c(6,7,8,9,10)]

df_new <- cbind(df_cat_new, df_cts)

df_new <- scale(df_new)

#clusters
k2 <- kmeans(df_new, centers = 2, nstart = 25)
k3 <- kmeans(df_new, centers = 3, nstart = 25)
k4 <- kmeans(df_new, centers = 4, nstart = 25)
k5 <- kmeans(df_new, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_new) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_new) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_new) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_new) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#get optimium # of clusters
#elbow method
set.seed(123)

fviz_nbclust(df_new, kmeans, method = "wss")

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df_new, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#avg siholette
fviz_nbclust(df_new, kmeans, method = "silhouette")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df_new, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_new))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#gap statistic
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df_new, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df_new, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df_new)