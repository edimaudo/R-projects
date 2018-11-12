#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'data.table', 'lubridate','skimr','rpart','stargazer')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Read dataset into R
diabetes <- read.csv(file.choose())
diabetes1 <- subset(diabetes,select=-c(encounter_id, patient_nbr, examide,citoglipton,weight, payer_code, medical_specialty)) 
diabetes2 <- diabetes1[diabetes1$race != "?",] 
diabetes2 <- diabetes2[diabetes2$diag_1 != "?",] 
diabetes2 <- diabetes2[diabetes2$diag_2 != "?",] 
diabetes2 <- diabetes2[diabetes2$diag_3 != "?",] 
# binary representation of readmitted within 30 days
diabetes2$readmittedbin <- ifelse(diabetes2$readmitted == "<30",1,0) 
# To make factor of levels
diabetes3 <- cbind(diabetes2[c(7:13,17)], lapply(diabetes2[c(1:6,14:16,18:44)],factor))

#visualization
# Plots of important data variables for the analysis
plot(diabetes3$diabetesMed,col ="lightgreen", xlab = "Diabetes Medicine", main= " Frequency of Diabetes Medicine", lwd =20,pch=18)
plot(diabetes3$readmitted,col ="lightgreen", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)
plot(diabetes3$readmittedbin,col ="lightgreen", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)


#modelling
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#split data into
# Testing-training data analysis
set.seed(111)
inTrain <- createDataPartition(diabetes3$readmittedbin, p=.2, list=FALSE)
objTrain <-diabetes3[inTrain,]
objTest <- diabetes3[-inTrain,]
table(objTrain$readmittedbin)
prop.table(table(objTrain$readmittedbin))
table(objTest$readmittedbin)
prop.table(table(objTest$readmittedbin))

#Prediction with three levels of response variable
cfit <- rpart(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method="class", minsplit = 20, minbucket = 5, cp = 0.001)
head(predict(cfit))
par(mar=c(1,1,0.25,1))
plot(cfit, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit, pretty = 0) 
rpart.predict <- predict(cfit, newdata = objTrain, type="class")
tail(rpart.predict)

#Prediction fit of the analysis
cf <-confusionMatrix(rpart.predict, objTrain$readmitted)
cf 
#Mean error rate
mean.error.rate.rpart <- 1- cf$overall[1]
mean.error.rate.rpart

# Validation of the tree
cfit.tree <- tree(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method="class")
cv.cfit.tree <- cv.tree(cfit.tree, FUN = prune.misclass)
cv.cfit.tree 
prune.cfit.tree <- prune.misclass(cfit.tree, best = 4)
#plot(prune.cfit.tree)
text(prune.cfit.tree, pretty = 0) 
# After pruning
cfit2 = prune(cfit, cp = 0.0014)
par(mar=c(1,1,0.25,1))
plot(cfit2, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit2)

#Prediction on test set
rpart.prune.predict <- predict(cfit2, newdata = objTest,type = "class")
cf.prune <-confusionMatrix(rpart.prune.predict,objTest$readmitted)
#Mean error rate
mean.error.rate.rpart.prune <- 1- cf.prune$overall[1]
mean.error.rate.rpart.prune

#roc curve
rpart.predict_bin <- predict(cfit_bin, newdata = objTrain,type="prob") 
View(rpart.predict_bin)accuracy.meas(objTrain$readmittedbin, rpart.predict_bin[,2]) 
roc.curve(objTrain$readmittedbin, rpart.predict_bin[,2], plotit = T)