#########################################################################################
                                      ## Load Libraries:
#########################################################################################
rm(list = ls())
library(tidyverse)
library(readxl)
library(caret)
library(fastDummies)
library(doParallel)
library(extraTrees)
# PCA analysis
library(FactoMineR)
library(factoextra)
library(gridExtra)


#########################################################################################
                                      ## Load Dataset:
#########################################################################################
crimes_data <- read.csv("number-of-crimes-committed-against-persons-disturbing-crimes-by-sex-age-marital-status-etc.csv", sep = ";")%>%
  rename(year = ï..Year) %>% # This can be removed according to the names given to Year after loading the dataset
  mutate(Age = as.numeric(Age)) %>% # Convert age to numerical
  dplyr::select(-Description.of.the.charge) %>% # Remove the description of the charge variable
  mutate_if(is.character, as.factor) # Convert character type variable to factor type

# Show the structure of the dataset
str(crimes_data)


#########################################################################################
                                      ## Data description:
#########################################################################################


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
# dev.off()


#########################################################################################
                                  ## Data processing:
#########################################################################################


crimes_data_model <- crimes_data %>%
  dummy_cols(c("Gender", "Marital.Status", 
               "Educational.Level", "Religion", "Job"),
             remove_first_dummy = TRUE) %>%
  dplyr::select(-c("Gender", "Marital.Status", "Educational.Level",
                   "Religion", "Job", "Number", "year")) %>%
  na.omit


str(crimes_data_model)


# RFE Method:
#-------------
library(doParallel)
library(caret)
registerDoSEQ()
control <- rfeControl(functions=rfFuncs,
                      method="cv", 
                      number=10)
# run the RFE algorithm
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
results <- rfe(crimes_data_model %>% select(-crime), 
               crimes_data_model$crime, sizes=c(1:8),
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



#########################################################################################
                                    ## PCA analysis:
#########################################################################################

library(FactoMineR)
library(factoextra)
library(gridExtra)

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


#########################################################################################
                                    ## Modeling:
#########################################################################################

set.seed(2020)

cl <- makePSOCKcluster(4) # Put here the number of cores in the Processor used for computation
models <- c("svmRadial", "xgbTree", "rf", "kknn", "nnet", "naive_bayes")



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

install.packages("ml_test")

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


library(cvms)

# Confusion matrix of the neural network algorithm:

model_train <- readRDS(paste0("model_", "nnet", ".RDS"))
y_pred <- predict(model_train, newdata = test_crimeData)
conf_mat <- confusion_matrix(targets = test_crimeData$crime,
                             predictions = y_pred)

# png("cm_nnet.png", width = 600, height = 600)
plot_confusion_matrix(conf_mat)
# dev.off()











