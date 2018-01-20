library(tidyverse)
library(neuralnet)
library(GGally)

#remove all data
rm(list=ls())

mydata <- read_csv(file.choose())

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#update data
mydata <- mydata %>%
  mutate(Age = scale01(Age), 
         Occupation = scale01(Occupation), 
         AddressTime = scale01(AddressTime), 
         JobTime = scale01(JobTime),
         PaymentHistory = scale01(PaymentHistory),
         CreditRisk = as.numeric(CreditRisk))

mydata <- mydata %>%
  mutate(CreditRisk = as.integer(CreditRisk), 
         CreditRisk = ifelse(CreditRisk == 1, TRUE, FALSE))

#split data
#index <- sample(1:nrow(mydata),round(0.90*nrow(mydata)))
train <- head(mydata,360)#mydata[index,]
test <- mydata[361:400,]
#build neural network
set.seed(123)
n <- names(train)
cred_risk <- neuralnet(CreditRisk ~ Age + MaritalStatus + Occupation  + Sex + AddressTime + 
                         JobTime + Checking + Savings + PaymentHistory + HomeOwnership,
                       data = train, hidden = c(3), linear.output = FALSE)
#show neural network
plot(cred_risk, rep = 'best')

#cross entropy error
# cred_risk_Train_Error <- cred_risk$result.matrix[1,1]
# paste("CE Error: ", round(cred_risk_Train_Error, 3))

#perform prediction on training data
prtrain.nn <- compute(cred_risk, train[,1:10])
prtrain.nn_ <- prtrain.nn$net.result*(max(mydata$CreditRisk)-min(mydata$CreditRisk))+min(mydata$CreditRisk)
train.r <- (train$CreditRisk)*(max(mydata$CreditRisk)-min(mydata$CreditRisk))+min(mydata$CreditRisk)
SSETrain.nn <- sum((train.r - prtrain.nn_)^2)#/nrow(train)
print(paste(SSETrain.nn))

#find accuracy of training
resultstraining <- data.frame(actual = train$CreditRisk, prediction = prtrain.nn$net.result)
roundedresultstraining<-sapply(resultstraining,round,digits=0)
roundedresultstrainingdf=data.frame(roundedresultstraining)
attach(roundedresultstrainingdf)
table(actual,prediction)


# perform prediction on test data
pr.nn <- compute(cred_risk,test[,1:10])
pr.nn_ <- pr.nn$net.result*(max(mydata$CreditRisk)-min(mydata$CreditRisk))+min(mydata$CreditRisk)
test.r <- (test$CreditRisk)*(max(mydata$CreditRisk)-min(mydata$CreditRisk))+min(mydata$CreditRisk)
MSETest.nn <- sum((test.r - pr.nn_)^2)/nrow(test)
print(paste(MSETest.nn))

#find accuracy of test
resultstest <- data.frame(actual = test$CreditRisk, prediction = pr.nn$net.result)
roundedresultstest<-sapply(resultstest,round,digits=0)
roundedresultstestdf=data.frame(roundedresultstest)
attach(roundedresultstestdf)
table(actual,prediction)

