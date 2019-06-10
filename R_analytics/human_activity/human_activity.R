library(randomForest)
library(gmodels)
library(neuralnet)
library(RSNNS)
library(Rcpp)
library(lattice)
library(ggplot2)
library(caret)
set.seed(123)

train_data<-read.table("train/X_train.txt")
train_lables<-read.table("train/Y_train.txt")

test_data<-read.table("test/X_test.txt")
test_lables<-read.table("test/Y_test.txt")

col_names <- readLines("features.txt")
colnames(train_data)<-make.names(col_names)
colnames(test_data)<-make.names(col_names)
colnames(train_lables)<-"label"
colnames(test_lables)<-"label"

model_mlp<-caret::train(lable~.,data=final_data[ttt,],method="mlp")
pre_mlp<-predict(model_mlp,final_data[-ttt,])
table(model_mlp,final_data[-ttt,1])
train_final<-cbind(train_lables,train_data)
test_final<-cbind(test_lables,test_data)
final_data<-rbind(train_final,test_final)
final_data$lable<-factor(final_data$lable)

model_mlpw<-caret::train(lable~.,data=final_data[ttt,],method="mlpWeightDecay")
pre_mlpw<-predict(model_mlpw,final_data[-ttt,])
table(pre_mlpw,final_data[-ttt,1])

model_pca<-caret::train(lable~.,data=final_data[ttt,],method="pcaNNet")
pre_pca<-predict(model_pca,final_data[-ttt,])
table(pre_pca,final_data[-ttt,1])

model_rf5<-randomForest(lable~.,final_data[ttt,])
pre_rf5<-predict(model_rf5,final_data[-ttt,],type = "response")
table(pre_rf5,final_data[-ttt,1])

model_knn<-train(lable~.,data=final_data[ttt,],method="knn")
pre_knn<-predict(model_knn,final_data[-ttt,])
table(pre_knn,final_data[-ttt,1])

data()
model_rfF<-randomForest(lable~.,final_data[1:7352,])
pre_rfF<-predict(model_rfF,final_data[-(1:7352),],type = "response")
table(pre_rfF,final_data[-(1:7352),1])

