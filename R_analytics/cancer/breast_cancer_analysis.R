#cancer info
# #  Attribute                     Domain
# -- -----------------------------------------
# 1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                      1 - 10
# 11. Class:                       (2 for benign, 4 for malignant)

#build model that can predict class

#libraries
library(caret)
library(corrplot)
library(ggplot2)
library(pROC)
library(LogicReg)
library(randomForest)

#data
data <- read.table("breast-cancer-wisconsin.data",header=F,sep=",",stringsAsFactors=F)

#summary
summary(data)

head(data)

names(data) <- c("id", 'ct','ucsize','ucshape','ma','secs','bn','bc','nn','miti','class')

#clean data
require(stringr)
#remove whitespace
data <-t(apply(data, 1, function(x) {str_replace(x, "\\s+", "")}))
data <-t(apply(data, 1, function(x) {str_replace(x, "\\D", NA)}))

#convert from string to numeric
data <- as.data.frame(data, stringsAsFactors=F)
#but they're still characters!
sapply(data,mode)
#transform them back to numeric
#define function
to_numeric <- function(x) as.numeric(as.character(x))
data <- modifyList(data, lapply(data, to_numeric))
sapply(data,mode)

data.orig <- data

#remove NA
data <- na.omit(data)

#exploratory analysis

#correlation
corinfo <- data[,2:11]
corrplot(cor(corinfo), method="number")


data[1] <- NULL #drop the first column
#create training and test data

#add column names
names(data) <- c('ct','ucsize','ucshape','ma','secs','bn','bc','nn','miti','class')

set.seed(1234)
splitIndex <- createDataPartition(data$class, p = .80,list = FALSE, times = 1)
trainSplit <- data[ splitIndex,]
testSplit <- data[-splitIndex,]
print(table(trainSplit$class))

#logistic regression
ctrl <- trainControl(method = "cv", number = 5)
modelglm <- train(as.factor(class) ~. , data = trainSplit, method = "glm", trControl = ctrl)
summary(modelglm)
### predict
predictors <- names(trainSplit)[names(trainSplit) != 'class']
predglm <- predict(modelglm, testSplit)
summary(predglm)
### score prediction using AUC
confusionMatrix(predglm, testSplit$class)

aucglm <- roc(as.numeric(testSplit$class), as.numeric(predglm),  ci=TRUE)
plot(aucglm, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Logistic Regression AUC:',round(aucglm$auc[[1]],3)),col = 'blue')

#decision trees
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(as.factor(class) ~., data = trainSplit, method = "C5.0Tree", trControl = ctrl)
pred <- predict(tbmodel, testSplit)
confusionMatrix(pred,testSplit$class)
roc(testdata, prediction)
aucdt <- roc(as.numeric(testSplit$class), as.numeric(pred))
print(aucdt)
plot(aucdt, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc1$auc[[1]],3)),col = 'blue')

#random forests
modelrf <- randomForest(as.factor(class) ~. , data = trainSplit)
importance(modelrf)

### predict
predrf <- predict(modelrf,testSplit)
summary(predrf)

### score prediction using AUC
confusionMatrix(predrf,testSplit$class)

aucrf <- roc(as.numeric(testSplit$class), as.numeric(predrf),  ci=TRUE)
plot(aucrf, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Random Forest AUC:',round(aucrf$auc[[1]],3)),col = 'blue')

#compare ROC curves 
plot(aucrf, ylim=c(0,1), main=paste('ROC Comparison : 
                                    RF(blue),decision tree(green),Logistic(Red)'),col = 'blue')
par(new = TRUE)
plot(aucglm,col = "red")
par(new = TRUE)
plot(aucdt,col="green")

#logistic and random forest do well so i'll go with random forest

#feature engineering
importance(modelrf)

#remove miti and ma
modelrf2 <- randomForest(as.factor(class) ~.-ma , 
                         data = trainSplit)
importance(modelrf2)

### predict
predrf2 <- predict(modelrf2,testSplit)
summary(predrf2)

confusionMatrix(predrf2,testSplit$class)

aucrf2 <- roc(as.numeric(testSplit$class), as.numeric(predrf2),  ci=TRUE)
plot(aucrf2, ylim=c(0,1), print.thres=TRUE, 
     main=paste('Random Forest AUC:',round(aucrf2$auc[[1]],3)),col = 'blue')

#pretty much the same as the original