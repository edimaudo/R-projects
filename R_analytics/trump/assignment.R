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

#===================
# Load data
#===================
df <- read_excel("Data.xlsx", sheet = "data")
df.backup <- df

#===================
# data summary
#===================
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
# replace NAs with 0
df[is.na(df)] <- 0

corr_data <- df %>%
  dplyr::select(party_reg, state_reg, pol_spectrum, religion, age, marital, 
                education, income,  ethnicity,vote,whovoted)

#=================
# Approach 2 - remove NAs
#=================
corr_data <- df %>%
  dplyr::select(party_reg, state_reg, pol_spectrum, religion, age, marital, 
                education, income,  ethnicity,vote,whovoted) %>%
  na.omit()

#=================
# Correlation
#=================
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
k1=4 # compare with multiple k values

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
chisq.test(corr_data$state_reg, corr_data$whovoted) 
chisq.test(corr_data$pol_spectrum, corr_data$whovoted) 
chisq.test(corr_data$religion, corr_data$whovoted)
chisq.test(corr_data$marital, corr_data$whovoted)
chisq.test(corr_data$education, corr_data$whovoted)
chisq.test(corr_data$income, corr_data$whovoted)
chisq.test(corr_data$ethnicity, corr_data$whovoted)
chisq.test(corr_data$vote, corr_data$whovoted)
## reject null hypothesis conclude the  two variables are, indeed, independent.

#=================
# Visualizations univariate
#=================
#whovoted
whovoted_plot <- corr_data %>%
  dplyr::filter(!(whovoted==0)) %>%
  group_by(whovoted) %>%
  dplyr::summarise(whovotedcount = n()) %>%
  dplyr::select(whovoted, whovotedcount) %>%
  ggplot(aes(x = reorder(as.factor(whovoted),whovotedcount), y = whovotedcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  ggtitle("Candidate Counts") + 
  xlab("Candidates") + 
  ylab("Count")
whovoted_plot
  
# Age
age_plot <- corr_data %>%
  ggplot(aes(x=age)) + geom_histogram(binwidth = 10, fill = "#0073C2FF") + 
  theme_minimal() + guides(fill = FALSE) + 
  guides(scale = 'none') + 
  ggtitle("Age plot") + 
  xlab("Age") + 
  ylab("Count")
age_plot

# Religion
religion_plot <- corr_data %>%
  dplyr::filter(!(religion==0)) %>%
  group_by(religion) %>%
  dplyr::summarise(religioncount = n()) %>%
  dplyr::select(religion, religioncount) %>%
  ggplot(aes(x = reorder(as.factor(religion),religioncount), y = religioncount)) + 
  geom_bar(stat = "identity", fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Religion") + 
  ylab("Count")
religion_plot

# Income
income_plot <- corr_data %>%
  dplyr::filter(!(income==0)) %>%
  group_by(income) %>%
  dplyr::summarise(incomecount = n()) %>%
  dplyr::select(income, incomecount) %>%
  ggplot(aes(x = reorder(as.factor(income),incomecount), y = incomecount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("income") + 
  ylab("Count")
income_plot

# Ethnicity
ethnicity_plot <- corr_data %>%
  dplyr::filter(!(ethnicity==0)) %>%
  group_by(ethnicity) %>%
  dplyr::summarise(ethnicitycount = n()) %>%
  dplyr::select(ethnicity, ethnicitycount) %>%
  ggplot(aes(x = reorder(as.factor(ethnicity),ethnicitycount), y = ethnicitycount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("ethnicity") + 
  ylab("Count")
ethnicity_plot

# Education
education_plot <- corr_data %>%
  dplyr::filter(!(education==0)) %>%
  group_by(education) %>%
  dplyr::summarise(educationcount = n()) %>%
  dplyr::select(education, educationcount) %>%
  ggplot(aes(x = reorder(as.factor(education),educationcount), y = educationcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("education") + 
  ylab("Count")
education_plot

# Party registration
party_reg_plot <- corr_data %>%
  dplyr::filter(!(party_reg==0)) %>%
  group_by(party_reg) %>%
  dplyr::summarise(party_regcount = n()) %>%
  dplyr::select(party_reg, party_regcount) %>%
  ggplot(aes(x = reorder(as.factor(party_reg),party_regcount), y = party_regcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("party_reg") + 
  ylab("Count")
party_reg_plot

# State registration
state_reg_plot <- corr_data %>%
  dplyr::filter(!(state_reg==0)) %>%
  group_by(state_reg) %>%
  dplyr::summarise(state_regcount = n()) %>%
  dplyr::select(state_reg, state_regcount) %>%
  ggplot(aes(x = reorder(as.factor(state_reg),state_regcount), y = state_regcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("state_reg") + 
  ylab("Count")
state_reg_plot

# Vote
vote_plot <- corr_data %>%
  dplyr::filter(!(vote==0)) %>%
  group_by(vote) %>%
  dplyr::summarise(votecount = n()) %>%
  dplyr::select(vote, votecount) %>%
  ggplot(aes(x = reorder(as.factor(vote),votecount), y = votecount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("vote") + 
  ylab("Count")
vote_plot

# Political spectrum
pol_spectrum_plot <- corr_data %>%
  dplyr::filter(!(pol_spectrum==0)) %>%
  group_by(pol_spectrum) %>%
  dplyr::summarise(pol_spectrumcount = n()) %>%
  dplyr::select(pol_spectrum, pol_spectrumcount) %>%
  ggplot(aes(x = reorder(as.factor(pol_spectrum),pol_spectrumcount), y = pol_spectrumcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("pol_spectrum") + 
  ylab("Count")
pol_spectrum_plot

#=================
# Visualizations bivariate
#=================

# Age vs who voted

# Religion vs whovoted

# income vs whovoted

# ethnicity vs whovoted

# education vs whovoted

# state reg vs whovoted

# party reg vs whovoted

# vote vs whovoted

# pol spectrum vs whovoted




