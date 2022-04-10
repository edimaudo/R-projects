#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl','grid','gridExtra')
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
df <- read.csv("offwhite.csv")

#===================
# Data Overview
#===================
summary(df)

# Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)


#===================
# Visualization
#===================
# Gender
df %>% 
  group_by(Gender) %>%
  summarize(Count= n()) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x = reorder(Gender, Count), y = Count, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(scale = "none") + 
  xlab("Gender") + 
  ylab("Count")

# Age 
df %>% 
  group_by(Age) %>%
  summarize(Count= n()) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x = reorder(Age, Count), y = Count, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(scale = "none") + 
  xlab("Gender") + 
  ylab("Count")

# Questionnaire likert scale
library(reshape2)
df_questionnaire <- df[,c(1:18)]
mb <- melt(df_questionnaire)

g2 <- ggplot()+
  geom_bar(data = mb, aes(x = reorder(Timestamp,value), y=value, fill=variable), 
           position="stack", stat="identity")+
  coord_flip() + 
  ylab("Score")+
  xlab("Questions")+
  theme(legend.position="bottom")

g2

#===================
# Cronbach alpha
#===================
library(ltm)
ltm::cronbach.alpha(df[, c("BA1","BA2","BA3")])