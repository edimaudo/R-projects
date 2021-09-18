rm(list = ls()) #clear environment
#===================
## Packages
#===================
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','plyr','MASS','readxl',
              'NbClust','scales','psy','factoextra','nFactors','GoodmanKruskal',
              'gridExtra','grid','caret','mlbench')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data
df <- read_excel("Data.xlsx", sheet = "data")
df <- df.backup
# data summary
summary(df)

# check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

# corr_data <- df %>%
#   dplyr::select(party_reg, primary_voter, pol_spectrum, party_id, party_salience, gov_trust, 
#                 better_economy, better_health, better_immigratino, better_taxes, better_environment, 
#                 fav_deathpen, stayhome, trust_media, corr_trump, covid_reopen, transgender, lgbtlaw, 
#                 birthright, deportkids, wall, russianinterfere, religion, age, marital, education, 
#                 spouse_edu, armedforces, labor, ethnicity, children, income, health, vote, whovoted, region)

#=================
# Approach 1 - set NAs to 0
#=================
df <- read_excel("Data.xlsx", sheet = "data")

# replace NAs with 0
df[is.na(df)] <- 0

#=================
# Correlation
#=================
corr_data <- df %>%
dplyr::select(party_reg, party_id,  religion, age, marital, education, ethnicity,vote,whovoted)

correlation_matrix = cor(corr_data)
# summarize the correlation matrix
print(correlation_matrix)
# plot correlation
corrplot(correlation_matrix, method = 'number', bg="#676767" ,tl.col='black', tl.cex=.75) 
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#=================
# using GoodmanKruskal R
#=================
corOutput <- GKtauDataframe(corr_data)
plot(corOutput)

#=================
# Clustering
#=================
set.seed(1)
fviz_nbclust(corr_data,kmeans,method = "silhouette")

# Use optimal no. of clusters in k-means #
k1=2 # compare with multiple k values

# Generate K-mean clustering
kfit <- kmeans(corr_data, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = corr_data, ellipse.type = "convex")+theme_minimal()

#=================
# chi square test
#=================
chisq.test(corr_data$party_reg, corr_data$whovoted) 
chisq.test(corr_data$party_id, corr_data$whovoted)
chisq.test(corr_data$religion, corr_data$whovoted)
chisq.test(corr_data$marital, corr_data$whovoted)
chisq.test(corr_data$education, corr_data$whovoted)
chisq.test(corr_data$ethnicity, corr_data$whovoted)
chisq.test(corr_data$vote, corr_data$whovoted)
## reject null hypothesis conclude the  two variables are, indeed, independent.

#=================
# Visualizations univariate and bivariate
#=================
#whovoted
whovoted_plot <- corr_data %>%
  group_by(whovoted) %>%
  dplyr::summarise(whovotedcount = n()) %>%
  dplyr::select(whovoted, whovotedcount) %>%
  ggplot(aes(x = reorder(as.factor(whovoted),whovotedcount), y = whovotedcount)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + theme_minimal() + 
  guides(fill = FALSE) + 
  ggtitle("Candidate Counts") + 
  xlab("Candidates") + 
  ylab("Count")
whovoted_plot
  
# Age
#Age vs whovoted

#Region
#Region vs whovoted

#income
# income vs whovoted

#ethnicity
# vs whovoted

#education
# vs whovoted

#religion
# vs whovoted

#state reg
# vs whovoted

#party reg
# vs whovoted

#vote
# vs whovoted

#pol spectrum
# vs whovoted



#=================
# Approach 2 remove all NAs
#=================
df <- read_excel("Data.xlsx", sheet = "data")

#=================
# Correlation
#=================
corr_data <- df %>%
  dplyr::select(party_reg, party_id,  religion, age, marital, education, ethnicity,vote,whovoted) %>%
  na.omit()

correlation_matrix = cor(corr_data)
# summarize the correlation matrix
print(correlation_matrix)
# plot correlation
corrplot(correlation_matrix, method = 'number', bg="#676767" ,tl.col='black', tl.cex=.75) 
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#=================
# using GoodmanKruskal R
#=================
corOutput <- GKtauDataframe(corr_data)
plot(corOutput)

#=================
# Clustering
#=================
set.seed(1)
fviz_nbclust(corr_data,kmeans,method = "silhouette")

# Use optimal no. of clusters in k-means #
k1=2 # compare with multiple k values

# Generate K-mean clustering
kfit <- kmeans(corr_data, k1, nstart=25, iter.max=1000, 
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
               trace=FALSE)

#Visualize clusters
fviz_cluster(kfit, data = corr_data, ellipse.type = "convex")+theme_minimal()
# looks like 2 distinct groups with some overlap

kfit$centers

#=================
# chi square test
#=================
chisq.test(corr_data$party_reg, corr_data$whovoted) 
chisq.test(corr_data$party_id, corr_data$whovoted)
chisq.test(corr_data$religion, corr_data$whovoted)
chisq.test(corr_data$marital, corr_data$whovoted)
chisq.test(corr_data$education, corr_data$whovoted)
chisq.test(corr_data$ethnicity, corr_data$whovoted)
chisq.test(corr_data$vote, corr_data$whovoted)

#=================
# Visualization univariate and bivariate
#=================
#whovoted

# Age
#Age vs whovoted

#Region
#Region vs whovoted

#income
# income vs whovoted

#ethnicity
# vs whovoted

#education
# vs whovoted

#religion
# vs whovoted

#state reg
# vs whovoted

#party reg
# vs whovoted

#pol spectrum
# vs whovoted