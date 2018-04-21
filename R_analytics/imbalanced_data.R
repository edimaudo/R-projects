
rareevent_boost <- read.table("churn.txt",sep="|", header=TRUE)
dmy<-dummyVars("~.",data=rareevent_boost)
rareeventTrsf<-data.frame(predict(dmy,newdata= rareevent_boost))
set.seed(10)
sub <- sample(nrow(rareeventTrsf), floor(nrow(rareeventTrsf) * 0.9))
sub1 <- sample(nrow(rareeventTrsf), floor(nrow(rareeventTrsf) * 0.1))
training <- rareeventTrsf [sub, ]
testing <- rareeventTrsf [-sub, ]
training_sub<- rareeventTrsf [sub1, ]
tables(training_sub)
head(training_sub)

#for unbalanced data set#
install.packages("unbalanced")
library(unbalanced)
data(ubIonosphere)
n<-ncol(rareevent_boost)
output<- rareevent_boost $CHURN_FLAG
output<-as.factor(output)
input<- rareevent_boost [ ,-n]
View(input)

#Balance the Dataset using ubSMOTE#
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE
                View(data)

#Balanced Data#
balancedData<-cbind(data$X,data$Y)
View(balancedData)
table(balancedData$CHURN_FLAG)

#Write the balanced data to be used to train the model#
write.table(balancedData,"balancedData.txt", sep="\t", row.names=FALSE)

#Build Boosting tree Model#
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
training <- repalceNAsWithMean(training)
testing <- repalceNAsWithMean(testing)

#Resampling Technique#
View(train_set)
fitcontrol<-trainControl(method="repeatedcv",number=10,repeats=1,verbose=FALSE)
gbmfit<-train(CHURN_FLAG~.,data=balancedData,method="gbm",verbose=FALSE)

#Score test Data#
testing$score_Y=predict(gbmfit,newdata=testing,type="prob")[,2]
testing$score_Y=ifelse(testing$score_Y>0.5,1,0)
head(testing,n=10)
write.table(testing,"D:/ Upasana/RareEvent /testing.txt", sep="\t", row.names=FALSE)
pred_GBM<-prediction(testing$score_Y,testing$CHURN_FLAG)

#Model Performance#
model_perf_GBM <- performance(pred_GBM, "tpr", "fpr")
model_perf_GBM1 <- performance(pred_GBM, "tpr", "fpr")
model_perf_GBM
pred_GBM1<-as.data.frame(model_perf_GBM)
auc.tmp_GBM <- performance(pred_GBM,"auc")
AUC_GBM <- as.numeric(auc.tmp_GBM@y.values)
auc.tmp_GBM