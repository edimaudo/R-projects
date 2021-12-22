rm(list = ls()) #clear environment

#===============
# Packages
#===============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','scales','dplyr','psy','nFactors',
              'MASS','psych','cluster','lattice','NbClust','gridExtra','doParallel','readxl')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===============
# Load data
#===============
brand <- read_excel("Brands.data.xlsx")
manufacture <- read_excel("Manufacture.data.xlsx")
opinion_survey <- read_excel("OpinionSurvey.data.xlsx")
pollution <- read_excel("Pollution.data.xlsx")
sympton <- read_excel("symptoms.data.xlsx")

#set seed
set.seed(1)

#summary(res.pca)
# Find PCA to plot
#layout(matrix(1:2, ncol=2))
#screeplot(res.pca)
#screeplot(res.pca, type="lines")

# Plot PCA
#fviz_pca_ind(res.pca)
#fviz_pca_ind(res.pca, label="none", habillage=pollutant$Cluster)

#==============
# Question 1
#==============

pollutant <- pollution[,c(2:10)]
res.pca <- prcomp(pollutant,  scale = TRUE)
# (a) Construct a PCA biplot of the Pollution data without showing alpha bags.
fviz_pca_biplot(res.pca)

# (b) Repeat (a) but instead of sample labels show the different groups (clusters) as 90% bags.
fviz_pca_biplot(res.pca2, 
                geom.ind = "point",
                habillage=pollution$Cluster,addEllipses=TRUE, ellipse.level=0.95)


# (c) Repeat (a) but give an optimal two-dimensional display of the correlations between the variables.
corr <- cor(pollutant)
corrplot(corr, method = 'number',bg="#D3D3D3")

# (d) Give a detailed interpretation of the plots constructed in (a), (b), and (c).

# (e) Construct a CVA biplot of the Pollution data with 90% bags added. Interpret and discuss the use of this biplot. 

# Question 2


# Question 3


# Question 4


# Question 5


# Question 6


# Question 7