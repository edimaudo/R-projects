#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
## Load Data
#===================
crimes_data <- read_excel("crimes-committed-against-women-ds.xlsx")

crimes_data <- crimes_data %>%
  mutate(Age = as.numeric(Age)) %>% # Convert age to numerical
  dplyr::select(-'Charge Descritpion') %>% # Remove charge description
  mutate_if(is.character, as.factor)# Convert character type variable to factor type

# review values
# unique(crimes_data$Year)
# unique(crimes_data$Gender)
# unique(crimes_data$Age)
# unique(crimes_data$`Marital Status`)
# unique(crimes_data$`Educational Level`)
# unique(crimes_data$Religion)
# unique(crimes_data$Job)
# unique(crimes_data$crime)

#crime per year
crimes_data %>%
  group_by(Year) %>%
  summarize(Number = sum(Counts)) %>%
  ggplot(aes(x = Year, y = Number)) + 
  geom_line(color = "#6d031c") +
  geom_point(color = "#a79086")+
  scale_x_continuous(breaks = seq(2007, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1000, 100)) + 
  guides(fill = FALSE) + 
  ggtitle("Evolution of the number of crimes") + 
  xlab("Year") + 
  ylab("Number of crimes")

#Age boxplot
# png("boxplot_age.png")
boxplot(crimes_data$Age, horizontal = TRUE, col = "lightblue", main = "Boxplot of the Age")
# dev.off()

#victims by nationality
crimes_data %>%
  group_by(Nationality) %>%
  summarize(Number = sum(Counts)) %>%
  ggplot(aes(x = reorder(Nationality, Number), y = Number, fill = Nationality)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of victims by Nationality") + 
  xlab("Nationality") + 
  ylab("Victims")

#victims by job
crimes_data %>% 
  group_by(Job) %>%
  summarize(Number = sum(Counts)) %>%
  arrange(desc(Number)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(Job, Number), y = Number, fill = Job)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of victims by Job") + 
  xlab("Job") + 
  ylab("Victims")

#victims by job category
# Show the crimes related to which jobs
crimes_data_job <- crimes_data %>%
  mutate(Job = as.character(Job)) %>%
  filter(Job %in% c("Not Specified", "Employee", "Maid", "Student", "Nanny", "House Wife"))

# Plot the heatmap
# png("heatmap.png")
pheatmap::pheatmap(data.matrix(table(crimes_data_job$Job, crimes_data_job$Target)),
                   treeheight_row = 0,
                   treeheight_col = 0)



#===================
#dummy variable set up
#===================
crimes_data_model2 <- crimes_data %>%
  dummy_cols(c("Marital Status", 
               "Education Level", "Religion", "Job", 'Nationality', 'Emirate'),
             remove_first_dummy = TRUE) %>%
  # dplyr::select(-c("Gender", "Marital.Status", "Educational.Level",
  #                  "Religion", "Job", "Number", "Year")) %>%
  dplyr::select(-c("Marital Status", "Gender",
                   "Education Level", "Religion", "Job", 'Nationality', 'Emirate',
                   'Age group','Counts')) %>%
  na.omit

crimes_data_model <- cbind(crimes_data_model2,crimes_data$Target)
colnames(crimes_data_model)[colnames(crimes_data_model) == 'crimes_data$crime'] <- 'crime'

#===================
#RFE
#===================
control <- rfeControl(functions=rfFuncs,
                      method="cv", 
                      number=10)
# run the RFE algorithm
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
results <- rfe(crimes_data_model2 %>% select(-Target), 
               crimes_data_model2$Target, sizes=c(1:8),
               rfeControl=control)
stopCluster(cl)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
# png("plot.png")
plot(results, type=c("g", "o"))
# dev.off()

#===================
#PCA
#===================
pca_crime <- prcomp(crimes_data_model2%>%dplyr::select(-c(Target)),
                    center = TRUE, 
                    scale = TRUE)

png("variance_plot.png")
fviz_eig(pca_crime, addlabels = TRUE,
         ylim = c(0, 2)) + 
  ggtitle("Variance over the different dimensions") + 
  theme(plot.title  = element_text(size = 20))
dev.off()


pca_table <- as.data.frame(pca_crime$x)

# Plot how the diagnosis variable is distributed along the first four dimensions:
dim_1 <- ggplot(pca_table, aes(x=PC1, fill=as.factor(crimes_data_model2$Target))) + 
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_2 <- ggplot(pca_table, aes(x=PC2, fill=as.factor(crimes_data_model2$Target))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_3 <- ggplot(pca_table, aes(x=PC3, fill=as.factor(crimes_data_model2$Target))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_4 <- ggplot(pca_table, aes(x=PC4, fill=as.factor(crimes_data_model2$Target))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")

# png("plot_pca.png", width = 1000, height = 800)
grid.arrange(dim_1, dim_2, dim_3, dim_4, nrow=2, ncol = 2)
# dev.off()


# 2D plot
# png("pca_2d.png", width = 1000, height = 1000)
fviz_pca_ind(pca_crime,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(crimes_data_model2$Target), # color by groups
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Crime type"
)
# dev.off()

#logistic regression
logistic_regression_model <- nnet::multinom(crime ~ ., 
                                            data = crimes_data_model2 %>% 
                                              select(Year, Age,`Job_House Wife`,`Job_Maid`,
                                                     `Marital Status_Single`,
                                                     Nationality_UAE, Target))
summary(logistic_regression_model)

#=================
#other models
#=================
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

model_svm <- train(
  as.factor(Target) ~.,
  data = train_crimeData, 
  method = "svmRadial",
  trControl = trainControl("cv",
                           number = 3)
)
saveRDS(model_svm, paste0("model_","svm",".RDS"))
stopCluster(cl)
y_pred <- predict(model_svm, newdata = test_crimeData)
cat(paste0("\nAccuracy of the model: ","svm"))
accuracy <- sum(y_pred == test_crimeData$Target)/nrow(test_crimeData)


#=================
#main model
#=================

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

crimes_data <- crimes_data[c(1:11)]

#check for missing information
crimes_data_model <- crimes_data %>%
  na.omit()
 
crime_info <- crimes_data_model %>%
  select(Year, Age)

crime_info2 <- crimes_data_model %>%
  select(Gender, `Marital Status`, `Education Level`, Religion,Job, Emirate,Nationality)

crime <- crimes_data_model %>%
  select(Target)

#combine data
df_cts <- as.data.frame(lapply(crime_info, normalize))
df_cat <- as.data.frame(lapply(crime_info2, labelEncoder))
df_new <- cbind(df_cts,df_cat,crime)

colnames(df_new) <- c("Year","Age","Gender","Marital.Status","Education.Level",
                  'Religion','Job','Emirate','Nationality','crime')



#tweak for sampling
#check for imbalance
print(table(df_new$crime))



#create train and test data
set.seed(2020)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#weight due to 
model_weights <- ifelse(train$crime == "Assault",
                        (1/table(train$crime)[1]) * 0.5,
                        (1/table(train$crime)[2]) * 0.5)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = FALSE)

#glm
fit.glm <- train(as.factor(crime)~., data=train, method="glm",family=binomial(), 
                 metric = "Accuracy", trControl = control, weights = model_weights)
#random forest
fit.rf <- train(as.factor(crime)~., data=train, method="rf", 
                metric = "Accuracy", trControl = control, weights = model_weights)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(crime)~., data=train, method="gbm", 
                 metric = "Accuracy", trControl = control, weights = model_weights)
#svm
fit.svm <- train(as.factor(crime)~., data=train, method="svmRadial", 
                 metric = "Accuracy", trControl = control, weights = model_weights)
#nnet
fit.nnet <- train(as.factor(crime)~., data=train, method="nnet", 
                  metric = "Accuracy", trControl = control, weights = model_weights)
#naive
fit.naive <- train(as.factor(crime)~., data=train, 
                   method="naive_bayes", metric = "Accuracy", 
                   trControl = control, weights = model_weights)
#extreme gradient boosting
fit.xgb <- train(as.factor(crime)~., data=train, 
                 method="xgbTree", metric = "Accuracy", 
                 trControl = control, weights = model_weights)
#bagged cart
fit.bg <- train(as.factor(crime)~., data=train, 
                method="treebag", metric = "Accuracy", 
                trControl = control, weights = model_weights)
#decision tree
fit.dtree <- train(as.factor(crime)~., data=train, 
                   method="C5.0", metric = "Accuracy", 
                   trControl = control, weights = model_weights)
#knn
fit.knn <- train(as.factor(crime)~., data=train, 
                 method="kknn", metric = "Accuracy", 
                 trControl = control, weights = model_weights)
#ensemble
fit.ensemble <- train(as.factor(crime)~., data=train, 
                      method="nodeHarvest", metric = "Accuracy", 
                      trControl = control, weights = model_weights)


stopCluster(cl)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, 
                          `gradient boost` = fit.gbm, 
                          `support vector machine` = fit.svm,
                          baggedCart = fit.bg, 
                          neuralnetwork = fit.nnet,
                          xgboost = fit.xgb, 
                          logisticregression = fit.glm, 
                          `decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive,
                          `ensemble` = fit.ensemble))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#other metrics


# Model accuracy
#mean(predicted.classes == test$crime)
#test data accuracy
# Make predictions
predicted.classes <- fit.dtree %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$crime, mode = "everything")

caret::varImp(fit.xgb)

#confusion matrix output
write.csv(as.data.frame(output$byClass),"output.csv")

#plot confusion matrix
library(ggplot2)     # to plot
library(gridExtra)   # to put more
library(grid)        # plot together

# plotting the matrix
output2 <- as.data.frame(output$table)
colnames(output2) <- c("Predicted",'Actual',"Freq")
cm_d_p <-  ggplot(data =output2, aes(x = Predicted , y =  Actual, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 8) +
  theme_light() +
  guides(fill=FALSE) 

cm_d_p
