# 2.4K tweets on US health care reform
# Labeled by humans as either positive or negative relative to a certain entity:
#   hcr, liberals, other, stupak, dems, teaparty, conservatives, gop, obama
# Possible task: Predict whether a tweet supports health care reform?
#   For building your model:
#   Can use the labels for training a classifier
# Can ignore the labels and use a lexicon
# Or a combination
# For assessing your model:
#   Use the labels to compute confusion matrix
# Goal of this project will be twofold:
#   To explain what makes a great model
# To show how people feel about HCR

# remove old data
rm(list=ls())

#load data
hcr <- read.csv(file.choose())

#add headers
colnames(hcr) <- c("tweet_id","other_info","sentiment","entity","tweet_info")

#create backup
hcr.backup <- hcr

#=================
#exploratory analysis
#=================
#entity
ggplot(hcr, aes(x=reorder(as.factor(entity), -table(entity)[entity]))) + 
  geom_bar(fill="steelblue")+
  theme_minimal()

#sentiment
ggplot(hcr, aes(x=reorder(as.factor(sentiment), -table(sentiment)[sentiment]))) + 
  geom_bar(fill="steelblue")+
  theme_minimal()

#=====================
#prediction
#=====================
#recode data

#check for correlation

#=============
#without text
#============
#create train and test data

#logistic regression
#decision trees
#LDA
#svm
#random forest
#neural network
#boosting algorithm

#===================
#compare models
#==================

#plot on ROC curve



#=================
#with text
#==================
#clean text function
#logistic regression
#decision trees
#LDA
#random forest
#neural network
#boosting algorithm

#compare models + plot on ROC curve


#===================
#sentiment analysis
#===================

#====================
#word cloud for each sentiment
#====================

#===================
#word clustering
#===================
