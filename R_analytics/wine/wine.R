#wine data

#libraries
library(psy)
library(nFactors)
library(MASS)
library(psych)
library(cluster)
library(ggplot2)
library(factoextra)
library(corrplot)
library(lattice)
library(readxl)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggpubr)
library(polycor) 

#Clear old data
rm(list=ls())

#use filechooser to select csv file
wine_data <- read.csv(file.choose(), sep = ',')

#Prepare data
wine_data.orig = mydata #save orig data copy
wine_data <- na.omit(wine_data) # listwise deletion of missing
#mydata[1] <- NULL #remove the first column
#mydata <- scale(mydata) #scale data if needed

#view data
View(wine_data)

#summary of data
summary(wine_data)

#PCA

corinfo <- wine_data[,2:14]
fit_pca <- princomp(cor(corinfo), cor=TRUE)
summary(fit_pca) # print variance accounted for
loadings(fit_pca) # pc loadings
plot(fit_pca,type="lines") # scree plot
#fit_pca$scores # the principal components
biplot(fit_pca)

#Perceptual map
pc.cr <-princomp(corinfo, cor = TRUE) #scale data
summary(pc.cr)
biplot(pc.cr)


