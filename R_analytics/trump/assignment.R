
rm(list = ls()) #clear environment

#===================
## Packages
#===================
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','plyr','MASS','readxl',
              'NbClust','scales','psy','factoextra','nFactors','GoodmanKruskal')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data
df <- read_excel("Data.xlsx", sheet = "data")
df.backup <- df
# data summary
summary(df)

#df <- na.omit(df)

# check for missing variables
#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

# replace NAs with 0
df[is.na(df)] <- 0

# Correlation
corr_data <- df %>%
  dplyr::select(party_reg, primary_voter, pol_spectrum, party_id, party_salience, gov_trust, 
         better_economy, better_health, better_immigratino, better_taxes, better_environment, 
         fav_deathpen, stayhome, trust_media, corr_trump, covid_reopen, transgender, lgbtlaw, 
         birthright, deportkids, wall, russianinterfere, religion, age, marital, education, 
         spouse_edu, armedforces, labor, ethnicity, children, income, health, vote, whovoted, region)

corMat = cor(corr_data)
corrplot(corMat, method = 'number', order = "hclust", tl.col='black', tl.cex=.75) 

# using GoodmanKruskal R
corOutput <- GKtauDataframe(corr_data)
plot(corOutput)

# Clustering
set.seed(1)
fviz_nbclust(corr_data,kmeans,method = "silhouette")

# Use optimal no. of clusters in k-means #
k1=3 # compare with multiple k values

# Generate K-mean clustering
kfit <- kmeans(corr_data, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = corr_data, ellipse.type = "convex")+theme_minimal()

# chi square test
