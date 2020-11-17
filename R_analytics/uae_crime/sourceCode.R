#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','ml_test')#,,'cvms', "rJava",'extraTrees')
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
crimes_data <- read.csv("uae_crime.csv", sep=";")

crimes_data <- crimes_data %>%
  mutate(Age = as.numeric(Age)) %>% # Convert age to numerical
  dplyr::select(-Description.of.the.charge) %>% # Remove the description of the charge variable
  mutate_if(is.character, as.factor) # Convert character type variable to factor type

# Show the structure of the dataset
str(crimes_data)

#===================
## Data exploration
#===================

# png("boxplot_age.png")
boxplot(crimes_data$Age, horizontal = TRUE, col = "lightblue", main = "Boxplot of the Age")
# dev.off()

# Data visualization:
# png("crimes_gender.png")
crimes_data %>%
  group_by(Gender) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = Gender, y = Number, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by gender")
# dev.off()


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>% # Graph showing the number of crimes by the marital status
  group_by(Marital.Status) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Marital.Status, Number), y = Number, fill = Marital.Status)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Marital Status") + 
  xlab("Marital Status") + 
  ylab("Number of crimes")

# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(Educational.Level) %>% # Graph showing the number of crimes by the Educational level
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Educational.Level, Number), y = Number, fill = Educational.Level)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Educational Level") + 
  xlab("Educational Level") + 
  ylab("Number of crimes")


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(Religion) %>% # Graph showing the number of crimes by religion
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Religion, Number), y = Number, fill = Religion)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Religion") + 
  xlab("Religion") + 
  ylab("Number of crimes")


# png("crimes_jobs.png")
crimes_data %>%
  group_by(Job) %>%
  summarize(Number = sum(Number)) %>%
  arrange(desc(Number)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(Job, Number), y = Number, fill = Job)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Job") + 
  xlab("Job") + 
  ylab("Number of crimes")
# dev.off()


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(crime) %>% # Graph showing the number of crimes by crime type
  summarize(Number = sum(Number)) %>%
  arrange(desc(Number)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(crime, Number), y = Number, fill = crime)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by crime type") + 
  xlab("Crime type") + 
  ylab("Number of crimes")




# Display the number of crimes over the years
png("evolution_crimes.png")
crimes_data %>%
  group_by(year) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = year, y = Number)) + 
  geom_line(color = "#6d031c") +
  geom_point(color = "#a79086")+
  scale_x_continuous(breaks = seq(2012, 2019, 1)) + 
  scale_y_continuous(breaks = seq(200, 1000, 200)) + 
  guides(fill = FALSE) + 
  ggtitle("Evolution of the number of crimes") + 
  xlab("Year") + 
  ylab("Number of crimes")
dev.off()


# Show the crimes related to which jobs
crimes_data_job <- crimes_data %>%
  mutate(Job = as.character(Job)) %>%
  filter(Job %in% c("Not Specified", "Employee", "Worker", "Student", "Driver", "House Wife"))

# Plot the heatmap
# png("heatmap.png")
pheatmap::pheatmap(data.matrix(table(crimes_data_job$Job, crimes_data_job$crime)),
                   treeheight_row = 0,
                   treeheight_col = 0)

#===================
## Data processing
#===================
crimes_data_model <- crimes_data %>%
  dummy_cols(c("Gender", "Marital.Status", 
               "Educational.Level", "Religion", "Job"),
             remove_first_dummy = TRUE) %>%
  # dplyr::select(-c("Gender", "Marital.Status", "Educational.Level",
  #                  "Religion", "Job", "Number", "Year")) %>%
  dplyr::select(-c("Gender", "Marital.Status", "Educational.Level",
                   "Religion", "Job", "Number","crime")) %>%
  na.omit

crimes_data_model <- cbind(crimes_data_model,crimes_data$crime)
colnames(crimes_data_model)[colnames(crimes_data_model) == 'crimes_data$crime'] <- 'crime'


str(crimes_data_model)

#-------------
# RFE Method
#-------------

#registerDoSEQ()
control <- rfeControl(functions=rfFuncs,
                      method="cv", 
                      number=10)
# run the RFE algorithm
#cl <- makePSOCKcluster(4)
#registerDoParallel(cl)
results <- rfe(crimes_data_model %>% select(-crime), 
               crimes_data_model$crime, sizes=c(1:8),
               rfeControl=control)
#stopCluster(cl)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
# png("plot.png")
plot(results, type=c("g", "o"))
# dev.off()

#-------------
# PCA Analysis
#-------------
pca_crime <- prcomp(crimes_data_model%>%dplyr::select(-c(crime)),
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
dim_1 <- ggplot(pca_table, aes(x=PC1, fill=as.factor(crimes_data_model$crime))) + 
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_2 <- ggplot(pca_table, aes(x=PC2, fill=as.factor(crimes_data_model$crime))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_3 <- ggplot(pca_table, aes(x=PC3, fill=as.factor(crimes_data_model$crime))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")
dim_4 <- ggplot(pca_table, aes(x=PC4, fill=as.factor(crimes_data_model$crime))) +
  geom_density(alpha=0.25) + 
  scale_fill_brewer(name = "Crime type", palette = "RdYlBu")

# png("plot_pca.png", width = 1000, height = 800)
grid.arrange(dim_1, dim_2, dim_3, dim_4, nrow=2, ncol = 2)
# dev.off()


# 2D plot
# png("pca_2d.png", width = 1000, height = 1000)
fviz_pca_ind(pca_crime,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(crimes_data_model$crime), # color by groups
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Crime type"
)
# dev.off()


#---------------
# New models ensemble & decision tree
#---------------
#ensemble
cl <- makePSOCKcluster(4)
registerDoParallel(cl) 

model_ensemble <- train(
  as.factor(crime) ~.,data = train_crimeData, 
  method ='nodeHarvest' ,trControl = trainControl("cv", 
                                                  number = 3))
saveRDS(model_ensemble, paste0("model_", "model_ensemble", ".RDS"))
stopCluster(cl)
y_pred <- predict(model_ensemble, newdata = test_crimeData)
cat(paste0("\nAccuracy of the model: ", "ensemble"))
accuracy <- sum(y_pred == test_crimeData$crime)/nrow(test_crimeData)

#decision tree  
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

model_decisiontree <- train(
  as.factor(crime) ~.,data = train_crimeData, 
  method = "C5.0Tree",trControl = trainControl("cv", 
                                               number = 3))
saveRDS(model_decisiontree, paste0("model_", "model_decisiontree", ".RDS"))
stopCluster(cl)
y_pred <- predict(model_decisiontree, newdata = test_crimeData)
cat(paste0("\nAccuracy of the model: ", "decision tree"))
accuracy <- sum(y_pred == test_crimeData$crime)/nrow(test_crimeData)

#-------------
# calculate correlation matrix
#-------------
# # calculate correlation matrix
correlationMatrix <- cor(crimes_data_model[,1:length(crimes_data_model)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#===================
## Modeling
#===================

# Logistic regression model:
logistic_regression_model <- glm(as.numeric(crime) ~ ., 
                                 data = crimes_data_model %>% 
                                   select(Gender_Male, Age,
                                          `Marital.Status_Not Specified`,
                                          Marital.Status_Single,
                                          crime))

summary(logistic_regression_model)

# Split into training and testing dataset:
set.seed(2020)
index_train <- createDataPartition(crimes_data_model$crime, p = 0.75)

train_crimeData <- crimes_data_model[index_train$Resample1, ]
test_crimeData <- crimes_data_model[-index_train$Resample1, ]

cl <- makePSOCKcluster(4) # Put here the number of cores in the Processor used for computation
models <- c("svmRadial", "xgbTree", "rf", "kknn",
            "nnet", "naive_bayes",'treebag','nodeHarvest')



registerDoParallel(cl) # We do parallel computing for faster computation

# Training the model
for(model in models){
  model_train <- train(
    as.factor(crime) ~., 
    data = train_crimeData, 
    method = model,
    trControl = trainControl("cv", 
                             number = 3)
  )
  saveRDS(model_train, paste0("model_", model, ".RDS"))
  cat(paste0("\nFinished: ", model))
  rm(model_train)
}
stopCluster(cl)

accuracies <- list()
cpt <- 1

# Computing the accuracies
for(model in models){
  model_train <- readRDS(paste0("model_", model, ".RDS"))
  y_pred <- predict(model_train, newdata = test_crimeData)
  accuracy <- sum(y_pred == test_crimeData$crime)/nrow(test_crimeData)
  accuracies[[cpt]] <- accuracy
  cpt <- cpt + 1
  cat(paste0("\nAccuracy of the model: ", model))
  print(accuracy)
  rm(model_train)
}

accuracies <- unlist(accuracies)

evaluation_data <- data.frame(
  Model = models,
  Accuracy = accuracies
)

# png("evaluation.png")
evaluation_data %>%
  ggplot(aes(x = reorder(Model, -Accuracy), y = Accuracy, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  guides(fill = FALSE) + 
  xlab("Model")
# dev.off()

# Confusion matrix of the neural network algorithm:
model_train <- readRDS(paste0("model_", "nnet", ".RDS"))
y_pred <- predict(model_train, newdata = test_crimeData)
conf_mat <- confusion_matrix(targets = test_crimeData$crime,
                             predictions = y_pred)

# png("cm_nnet.png", width = 600, height = 600)
plot_confusion_matrix(conf_mat)
# dev.off()


# dev.off()

#model update


#------------------
## model update
#------------------





  
#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
  
crime_info <- crimes_data[,c(1,3)]
crime_info2 <- crimes_data[,c(2,4,5,6,7,8)]
  
df_cts <- as.data.frame(lapply(crime_info, normalize))
df_cat <- as.data.frame(lapply(crime_info2, labelEncoder))
  
df_new <- cbind(df_cts,df_cat)
  
set.seed(2020)
  
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)
  
cl <- makePSOCKcluster(4) # Put here the number of cores in the Processor used for computation
registerDoParallel(cl) # We do parallel computing for faster computation
models <- c("svmRadial", "xgbTree", "rf", "kknn",
              "nnet", "naive_bayes",'treebag','nodeHarvest','C5.0Tree')
# model traning
for(model in models){
  model_train <- train(
    as.factor(crime) ~.,data = train, 
    method = model,trControl = trainControl("cv", 
                             number = 3)
  )
  saveRDS(model_train, paste0("model_", model, ".RDS"))
  cat(paste0("\nFinished: ", model))
  rm(model_train)
}

stopCluster(cl)

accuracies <- list()
cpt <- 1

# Computing the accuracies
for(model in models){
  model_train <- readRDS(paste0("model_", model, ".RDS"))
  y_pred <- predict(model_train, newdata = test)
  accuracy <- sum(y_pred == test$crime)/nrow(test)
  accuracies[[cpt]] <- accuracy
  cpt <- cpt + 1
  cat(paste0("\nAccuracy of the model: ", model))
  print(accuracy)
  rm(model_train)
}

accuracies <- unlist(accuracies)
evaluation_data <- data.frame(
  Model = models,
  Accuracy = accuracies
)

# png("evaluation.png")
evaluation_data %>%
  ggplot(aes(x = reorder(Model, -Accuracy), y = Accuracy, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  guides(fill = FALSE) + 
  xlab("Model")
# dev.off()


#checking for crime variable imbalance
df_crimes <- crimes_data %>%
  group_by(crime) %>%
  summarise(crime_count = n()) %>%
  select(crime, crime_count)

ggplot(data = df_crimes,aes(x = as.factor(crime),y = crime_count)) +
  geom_bar(stat = "identity", width = 0.3) + theme_light() +
  labs(x = "Crime",
       y = "Amount of crime") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#data is skewed to thefting, assault, burglary, murder

#try and fix data imbalance
library(ROSE)
BalancedData <- ovun.sample(MinorityClassi~, 
                            ImbalancedData, method="over", p=0.5, 
                            subset=options("subset")$subset, 
                            na.action=options("na.action")$na.action, seed)
index = createDataPartition(y=BalancedData$Class, p=0.7, list=FALSE)
train = BalancedData[index,]
test = BalancedData[-index,]
BalTrain <- droplevels.data.frame(train) 
BalTest <- droplevels.data.frame(test)

