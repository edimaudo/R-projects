

#data imputation
#Imputing missing values using KNN.Also centering and scaling numerical columns
#preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))

#library('RANN')
#train_processed <- predict(preProcValues, train)
#sum(is.na(train_processed))


#Remove Redundant Feature remove when absolute correlation >= 0.75
#set.seed(7)
# # load the library
# library(mlbench)
# library(caret)
# # load the data
# data(PimaIndiansDiabetes)
# # calculate correlation matrix
# correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)

#rank features
# set.seed(7)
# # load the library
# library(mlbench)
# library(caret)
# # load the dataset
# data(PimaIndiansDiabetes)
# # prepare training scheme
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)

#feature selection
# load the library
# library(mlbench)
# library(caret)
# # load the data
# data(PimaIndiansDiabetes)
# # define the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))


#feature selection 2
#Feature selection using rfe in caret
# control <- rfeControl(functions = rfFuncs,
#                       method = "repeatedcv",
#                       repeats = 3,
#                       verbose = FALSE)
# outcomeName<-'Loan_Status'
# predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
# Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
#                          rfeControl = control)

#split data into training and test
# library(caTools)
# set.seed(123)
# df.new$final_result <- as.factor(df.new$final_result)
# sample <- sample.split(df,SplitRatio = 0.75)
# train <- subset(df.new,sample ==TRUE)
# test <- subset(df.new, sample==FALSE)





#------------------------------
#modelling approach
#svm, xgboost, random forest, bagging, boosting
#------------------------------
#find important columns
#names(getModelInfo())

#Application of a Machine Learning Algorithm to training dataset
# model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
# model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
# model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
# model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')

#Setup a ML algorithm and parameter settings
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 5,
#   repeats = 5)

#remember to add verbose=FALSE to avoid endless outputs

#fine tune model
#modelLookup(model='gbm')
#Creating grid
#grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),
interaction.depth=c(1,5,10))
# training the model
#model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)
# summarizing the model
#print(model_gbm)

#using tune length
#using tune length
#model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
#print(model_gbm)

#Checking variable importance for GBM
#Variable Importance
#varImp(object=model_gbm)

#Evaluation of trained Algorithm (or Model) and result
#Predictions
#predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
#table(predictions)
#confusionMatrix(predictions,testSet[,outcomeName])
#test model on test data
