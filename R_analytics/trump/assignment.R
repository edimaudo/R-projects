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
  filter(whovoted %in% c(1,2)) %>%
  mutate(whovoted = recode(whovoted, "1" = "Biden","2" = "Trump"),
         vote = recode(vote, "1" = "Yes","2" = "No","0" = "0"),
         ethnicity = recode(ethnicity, "1" = "White, non-Hispanic","2" = "Black, non-Hispanic",
                            "3" = "Hispanic","4" = "Asian or Native Hawaiian",
                            "5" = "Native American","6" = "Multiple","0" = "0"),
         income = recode(income,"1" = "Below $10k","2" = "$10k - $15k",
                         "3" = "$15k - $20k","4" = "$20k - $25k",
                         "5" = "$25k - $30k","6" = "$30k - $35k",
                         "7" = "$35k - $40k","8" = "$40k - $45k",
                         "9" = "$45k - %50ik","10" = "$50k - $60k",
                         "11" = "$60k - $65k","12" = "$65k - $70k",
                         "13" = "$70k - $75k","14" = "$75k - $80k",
                         "15" = "$80k - $90k","16" = "$90k - $100k",
                         "17" = "$100k - $110k","18" = "$110k - $125k",
                         "19" = "$125k - $150k","20" = "$150k - $175k",
                         "21" = "$175k - $250k","22" = "$250k +","0" = "0"),
         education = recode(education,"1" = "Less than high school","2" = "High school credential",
                            "3" = "Some post-high school, no bachelor's degree","4" = "Bachelor's degree",
                            "5" = "Graduate degree","0" = "0"),
         marital = recode(marital,"1" = "Married: Spouse Present","2" = "Married: Spouse Absent",
                          "3" = "Widowed","4" = "Divorced",
                          "5" = "Separated","6" = "Never Married","0" = "0"),
         religion = recode(religion,"1" = "Mainline Protestant","2" = "Black Protestant",
                           "3" = "Undifferentiated Protestant","4" = "Undifferentiated Protestant",
                           "5" = "Roman Catholic","6" = "Other Christian",
                           "7" = "Jewish","8" = "Other",
                           "9" = "Non-Religious","0" = "0"),
         pol_spectrum  = recode(pol_spectrum,"1" = "Extremely liberal","2" = "Liberal",
                           "3" = "Slightly liberal","4" = "Moderate",
                           "5" = "Slightly conservative","6" = "Conservative",
                           "7" = "Extremely conservative","0" = "0"),
         party_reg = recode(party_reg,"1" = "Democrat","2" = "Republican",
                            "4" = "None or 'independent'","4" = "Other","0" = "0")
         
         ) %>%
  dplyr::select(party_reg, pol_spectrum, religion, age, marital, 
                education, income,  ethnicity,vote,whovoted)
  




#=================
# Approach 2 - remove NAs
#=================
# corr_data <- df %>%
#   dplyr::select(party_reg, pol_spectrum, religion, age, marital, 
#                 education, income,  ethnicity,vote,whovoted) %>%
#   na.omit()

#=================
# Correlation
#=================
# correlation_matrix = cor(corr_data)
# # summarize the correlation matrix
# print(correlation_matrix)
# # plot correlation
# corrplot(correlation_matrix, method = 'number', bg="#676767" ,tl.col='black', tl.cex=.75) 
# # # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.75)
# # # print indexes of highly correlated attributes
# print(highlyCorrelated)

#=================
# using GoodmanKruskal R
#=================
# corOutput <- GKtauDataframe(corr_data)
# plot(corOutput)

#=================
# Clustering
#=================
# set.seed(1)
# fviz_nbclust(corr_data,kmeans,method = "silhouette")
# 
# # Use optimal no. of clusters in k-means #
# k1=4 # compare with multiple k values
# 
# # Generate K-mean clustering
# kfit <- kmeans(corr_data, k1, nstart=25, iter.max=1000, 
#                algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
#                trace=FALSE)
# 
# #Visualize clusters
# fviz_cluster(kfit, data = corr_data, ellipse.type = "convex")+theme_minimal()

#=================
# chi square test
#=================
chisq.test(corr_data$party_reg, corr_data$whovoted) 
chisq.test(corr_data$pol_spectrum, corr_data$whovoted) 
chisq.test(corr_data$religion, corr_data$whovoted)
chisq.test(corr_data$marital, corr_data$whovoted)
chisq.test(corr_data$education, corr_data$whovoted)
chisq.test(corr_data$income, corr_data$whovoted)
chisq.test(corr_data$ethnicity, corr_data$whovoted)
#chisq.test(corr_data$vote, corr_data$whovoted)
## reject null hypothesis conclude the  two variables are, indeed, independent for all except
#income

#=================
# Visualizations univariate trump and biden
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
  filter(!age==0) %>%
  ggplot(aes(x=age)) + geom_histogram(binwidth = 10,fill = "#0073C2FF") + #color #fill='white'
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
# state_reg_plot <- corr_data %>%
#   dplyr::filter(!(state_reg==0)) %>%
#   group_by(state_reg) %>%
#   dplyr::summarise(state_regcount = n()) %>%
#   dplyr::select(state_reg, state_regcount) %>%
#   ggplot(aes(x = reorder(as.factor(state_reg),state_regcount), y = state_regcount)) + 
#   geom_bar(stat = "identity",fill = "#0073C2FF") + 
#   coord_flip() + theme_minimal() + 
#   guides(scale = 'none') + 
#   xlab("state_reg") + 
#   ylab("Count")
# state_reg_plot

# Vote
# vote_plot <- corr_data %>%
#   dplyr::filter(!(vote==0)) %>%
#   group_by(vote) %>%
#   dplyr::summarise(votecount = n()) %>%
#   dplyr::select(vote, votecount) %>%
#   ggplot(aes(x = reorder(as.factor(vote),votecount), y = votecount)) + 
#   geom_bar(stat = "identity",fill = "#0073C2FF") + 
#   coord_flip() + theme_minimal() + 
#   guides(scale = 'none') + 
#   xlab("vote") + 
#   ylab("Count")
# vote_plot

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
  xlab("Political spectrum") + 
  ylab("Count")
pol_spectrum_plot

# Marital Status
marital_plot <- corr_data %>%
  dplyr::filter(!(marital==0)) %>%
  group_by(marital) %>%
  dplyr::summarise(maritalcount = n()) %>%
  dplyr::select(marital, maritalcount) %>%
  ggplot(aes(x = reorder(as.factor(marital),maritalcount), y = maritalcount)) + 
  geom_bar(stat = "identity",fill = "#0073C2FF") + 
  coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Marital Status") + 
  ylab("Count")
marital_plot

#=================
# Visualizations bivariate
#=================
# Religion vs whovoted
religion_who_plot <- corr_data %>%
  dplyr::filter(!(religion==0)) %>%
  dplyr::arrange(religion) %>%
  ggplot(aes(x = as.factor(religion), 
           fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + 
  guides(scale = 'none') + coord_flip() + theme_minimal() + 
  xlab("Religion") + labs(fill = "Who Voted") +
  ylab("Count")
religion_who_plot

# income vs whovoted
income_who_plot <- corr_data %>%
  dplyr::filter(!(income==0)) %>%
  dplyr::arrange(desc(income)) %>%
ggplot(aes(x = as.factor(income), 
                      fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Income") + labs(fill = "Who voted") +
  ylab("Count")
income_who_plot

# ethnicity vs whovoted
ethincity_who_plot <- corr_data %>%
  dplyr::filter(!(ethnicity==0)) %>%
ggplot(aes(x = as.factor(ethnicity), 
                      fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Ethnicity") + labs(fill = "Who voted") +
  ylab("Count")
ethincity_who_plot 

# education vs whovoted
education_who_plot <- corr_data %>%
  dplyr::filter(!(education==0)) %>%
  dplyr::arrange(desc(education)) %>%
ggplot(aes(x = as.factor(education), 
                      fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() +  
  guides(scale = 'none') + 
  xlab("Education") + labs(fill = "Who voted") +
  ylab("Count")
education_who_plot

# state reg vs whovoted
# state_reg_who_plot <- corr_data %>%
#   dplyr::filter(!(state_reg==0)) %>%
#   dplyr::filter(whovoted %in% c(1,2)) %>%
# ggplot(aes(x = as.factor(state_reg), 
#                       fill = as.factor(whovoted))) + 
#   geom_bar(position = "stack") + theme_minimal() + 
#   guides(scale = 'none') + 
#   xlab("State Registration") + labs(fill = "Who voted") +
#   ylab("Count")

# party reg vs whovoted
party_reg_who_plot <- corr_data %>%
  dplyr::filter(!(party_reg==0)) %>%
  dplyr::arrange(desc(party_reg)) %>%
ggplot(aes(x = as.factor(party_reg), 
                      fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() +  
  guides(scale = 'none') + 
  xlab("party Registration") + labs(fill = "Who voted") +
  ylab("Count")
party_reg_who_plot 

# Marital status vs who voted
marital_who_plot <- corr_data %>%
  dplyr::filter(!(marital==0)) %>%
  dplyr::arrange(desc(marital)) %>%
  ggplot(aes(x = as.factor(marital), 
             fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() +  
  guides(scale = 'none') + 
  xlab("Marital Status") + labs(fill = "Who voted") +
  ylab("Count")
marital_who_plot

# pol spectrum vs whovoted
pol_spec_who_plot <- corr_data %>%
  dplyr::filter(!(pol_spectrum==0)) %>%
  dplyr::arrange(desc(pol_spectrum)) %>%
  ggplot(aes(x = as.factor(pol_spectrum), 
             fill = as.factor(whovoted))) + 
  geom_bar(position = "dodge") + coord_flip() + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Political Spectrum") + labs(fill = "Who voted") +
  ylab("Count")
pol_spec_who_plot

# Age vs who voted
age_who_plot <- corr_data %>%
  dplyr::filter(!(age==0)) %>%
  ggplot(aes(x = age, 
             fill = as.factor(whovoted))) + 
  geom_histogram(binwidth = 5) + theme_minimal() + 
  guides(scale = 'none') + 
  xlab("Age") + labs(fill = "Who voted") +
  ylab("Count")
age_who_plot



grid.arrange(religion_who_plot,income_who_plot,ethincity_who_plot,education_who_plot,
             party_reg_who_plot,pol_spec_who_plot,age_who_plot,
             ncol=4, nrow=2)
