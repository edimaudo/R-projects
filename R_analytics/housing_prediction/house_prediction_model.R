#Predicting house prices

#load libraries
library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)

#load data
Training <- read.csv("train.csv")
Test <- read.csv("test.csv")

#set up work process
#Data cleaning, Descriptive Analysis, Model Selection, Final Prediction

#data cleaning
Num_NA<-sapply(Training,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(Training),Count=Num_NA)

#remove columns that have high number of missing values
Training<- Training[,-c(7,73,74,75)]

#ransferred dummny variables into numeric form
# Numeric Variables
Num<-sapply(Training,is.numeric)
Num<-Training[,Num]

for(i in 1:77){
  if(is.factor(Training[,i])){
    Training[,i]<-as.integer(Training[,i])
  }
}

# Test
Training$Street[1:50]

Training[is.na(Training)]<-0
Num[is.na(Num)]<-0

#Descriptive analysis

#correlation of numerical values
correlations<- cor(Num[,-1],use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

pairs(~SalePrice+OverallQual+TotalBsmtSF+GarageCars+GarageArea,data=Training,
      main="Scatterplot Matrix")

#SalePrice) looks having decent linearity when plotting with other variables. 
#However, it is also obvious that some independent 
#variables also have linear relationship with others

p<- ggplot(Training,aes(x= YearBuilt,y=SalePrice))+geom_point()+geom_smooth()
p

#model selection
#split the training dataset in the ration 6:4
Training_Inner<- Training[1:floor(length(Training[,1])*0.6),]
Test_Inner<- Training[(length(Training_Inner[,1])+1):1460,]

#linear regression
#R Square is not bad, but many variables do not pass the Hypothesis Testing, 
#so the model is not perfect. 
#Potential overfitting will occur if someone insist on using it
reg1<- lm(SalePrice~., data = Training_Inner)
summary(reg1)

#based on the previous summary unnecessary parameters were removed
#modified linear regression
reg1_Modified_2<-lm(formula = SalePrice ~ MSSubClass + LotArea + 
                      Condition2 + OverallQual + OverallCond + 
                      YearBuilt  + RoofMatl +  ExterQual + 
                      BsmtQual + BsmtCond + BsmtFinSF1 + BsmtFinSF2 + 
                      BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr + 
                      KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
                      GarageYrBlt + GarageCars +  SaleCondition, 
                    data = Training_Inner)
summary(reg1_Modified_2)

#all values pass the hypothesis tests and the residuals aren't that bad

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(reg1_Modified_2)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#check performance of model
Prediction_1<- predict(reg1_Modified_2, newdata= Test_Inner)
#rmse(log(Test_Inner$SalePrice),log(Prediction_1))

#model 2 - lasso regression - to remove multicollinearity
Independent_variable<- as.matrix(Training_Inner[,1:76])
Dependent_Variable<- as.matrix(Training_Inner[,77])
laa<- lars(Independent_variable,Dependent_Variable,type = 'lasso')
plot(laa)

best_step<- laa$df[which.min(laa$Cp)]
Prediction_2<- predict.lars(laa,newx =as.matrix(Test_Inner[,1:76]), s=best_step, type= "fit")
#rmse(log(Test_Inner$SalePrice),log(Prediction_2$fit))

#random forest model
for_1<- randomForest(SalePrice~.,data= Training_Inner)
Prediction_3 <- predict(for_1, newdata= Test_Inner)
#rmse(log(Test_Inner$SalePrice),log(Prediction_3))

#final prediction
# Transforming Test set #
Test<- Test[,-c(7,73,74,75)]
for(i in 1:76){
  if(is.factor(Test[,i])){
    Test[,i]<-as.integer(Test[,i])
  }
}
Test[is.na(Test)]<-0

#transform into viable format
re_train<- as.matrix(Training,rownames.force =NA)
re_train<- as(re_train,'sparseMatrix')
retrain_Data<- xgb.DMatrix(data = re_train[,2:76],label=re_train[,"SalePrice"])
bstSparse_retrain<- xgb.train(params=param,
                              data=retrain_Data,
                              nrounds = 600,
                              watchlist = list(train = retrain_Data),
                              verbose = TRUE,
                              print_every_n = 50,
                              nthread = 2
)

Test_Matrix<-as.matrix(Test,rownames.force = FALSE)
Test_Matrix<-as(Test_Matrix,"sparseMatrix")
Test_Matrix<-xgb.DMatrix(data = Test_Matrix[,2:76])

#submission
#Submit<- predict(Training, newdata=Test_Matrix)
#Submit<-data.frame(Id= Test$Id, SalePrice= Submit)