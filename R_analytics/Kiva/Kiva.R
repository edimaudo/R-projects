# remove old data
rm(list=ls())

#load data
kiva_info <- read.csv(file.choose())

#backup file
kiva.backup <- kiva_info

#====================
#exploratory analysis
#====================
#status
# ggplot(kiva_info, aes(x=reorder(as.factor(status), -table(status)[status]))) + 
#   geom_bar(fill="steelblue")+
#   theme_minimal()
# 
# #sector
# ggplot(kiva_info, aes(x=reorder(as.factor(sector), -table(sector)[sector]))) + 
#   geom_bar(fill="red")+
#   theme_minimal()
# 
# #country
# ggplot(kiva_info, aes(x=reorder(as.factor(country), -table(country)[country]))) + 
#   geom_bar(fill="green")+
#   theme_minimal()
# 
# #gender
# ggplot(kiva_info, aes(x=reorder(as.factor(gender), -table(gender)[gender]))) + 
#   geom_bar(fill="purple")+
#   theme_minimal()
# 
# #loan amount
# ggplot(kiva_info, aes(loan_amount)) + geom_histogram() + theme_minimal()
# 
# #non payment
# ggplot(kiva_info, aes(x=reorder(as.factor(nonpayment), -table(nonpayment)[nonpayment]))) + 
#   geom_bar(fill="steelblue")+
#   theme_minimal()

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

#==================
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

#==================
#topic clustering
#==================