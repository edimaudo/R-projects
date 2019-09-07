#remove old data
rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','ggfortify)')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = FALSE)

glimpse(df)

#data cleaning
df$diagnosis <- as.factor(df$diagnosis)

df <- df %>% rename(concave_points_mean = `concave points_mean`, 
                    concave_points_se = `concave points_se`, 
                    concave_points_worst = `concave points_worst`)

summary(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check for data imbalance in response variable
round(prop.table(table(df$diagnosis)), 2)

#correlation
df_corr <- cor(df %>% select(-id, -diagnosis))
corrplot::corrplot(df_corr, order = "hclust", tl.cex = 1, addrect = 8)


# The findcorrelation() function from caret package remove highly correlated predictors
# based on whose correlation is above 0.9. This function uses a heuristic algorithm 
# to determine which variable should be removed instead of selecting blindly
df2 <- df %>% select(-findCorrelation(df_corr, cutoff = 0.9))

#Number of columns for our new data frame
ncol(df2)

#other feature selection
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_corr[,1:31], df_corr[,32], sizes=c(1:31), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#preprocessing of data
#using pca
preproc_pca_df <- prcomp(df %>% select(-id, -diagnosis), scale = TRUE, center = TRUE)
summary(preproc_pca_df)

# Calculate the proportion of variance explained
pca_df_var <- preproc_pca_df$sdev^2
pve_df <- pca_df_var / sum(pca_df_var)
cum_pve <- cumsum(pve_df)
pve_table <- tibble(comp = seq(1:ncol(df %>% select(-id, -diagnosis))), pve_df, cum_pve)

ggplot(pve_table, aes(x = comp, y = cum_pve)) + 
  geom_point() + 
  geom_abline(intercept = 0.95, color = "red", slope = 0)

preproc_pca_df2 <- prcomp(df2, scale = TRUE, center = TRUE)
summary(preproc_pca_df2)

pca_df2_var <- preproc_pca_df2$sdev^2

# proportion of variance explained
pve_df2 <- pca_df2_var / sum(pca_df2_var)
cum_pve_df2 <- cumsum(pve_df2)
pve_table_df2 <- tibble(comp = seq(1:ncol(df2)), pve_df2, cum_pve_df2)

ggplot(pve_table_df2, aes(x = comp, y = cum_pve_df2)) + 
  geom_point() + 
  geom_abline(intercept = 0.95, color = "red", slope = 0)

#visualize first two componenets
autoplot(preproc_pca_df2, data = df,  colour = 'diagnosis',
loadings = FALSE, loadings.label = TRUE, loadings.colour = "blue")

#visualize 1st 3 components
df_pcs <- cbind(as_tibble(df$diagnosis), as_tibble(preproc_pca_df2$x))
GGally::ggpairs(df_pcs, columns = 2:4, ggplot2::aes(color = value))


#using lda
preproc_lda_df <- MASS::lda(diagnosis ~., data = df, center = TRUE, scale = TRUE)
preproc_lda_df

# Making a df out of the LDA for visualization purpose.
predict_lda_df <- predict(preproc_lda_df, df)$x %>% 
  as_data_frame() %>% 
  cbind(diagnosis = df$diagnosis)


#data modeling
set.seed(1815)
df3 <- cbind(diagnosis = df$diagnosis, df2)
df_sampling_index <- createDataPartition(df3$diagnosis, times = 1, p = 0.8, list = FALSE)
df_training <- df3[df_sampling_index, ]
df_testing <-  df3[-df_sampling_index, ]
df_control <- trainControl(method="cv",
                           number = 15,
                           repeats = 3, 
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

#logistic regression
model_logreg_df <- train(diagnosis ~., data = df_training, method = "glm", 
                         metric = "ROC", preProcess = c("scale", "center"), 
                         trControl = df_control)

prediction_logreg_df <- predict(model_logreg_df, df_testing)
cm_logreg_df <- confusionMatrix(prediction_logreg_df, df_testing$diagnosis, positive = "M")
cm_logreg_df


#random forest
model_rf_df <- train(diagnosis ~., data = df_training,
                     method = "rf", 
                     metric = 'ROC', 
                     trControl = df_control)

prediction_rf_df <- predict(model_rf_df, df_testing)
cm_rf_df <- confusionMatrix(prediction_rf_df, df_testing$diagnosis, positive = "M")
cm_rf_df

#feature importance
varImpPlot(model_rf_df$finalModel)

model_rf2_df <- train(diagnosis ~., data = df_training, 
                      method = "ranger", 
                      metric = "ROC", 
                      preProcess = c("scale", "center"), 
                      trControl = df_control)
prediction_rf2_df <- predict(model_rf2_df, df_testing)
confusionMatrix(prediction_rf2_df, df_testing$diagnosis, positive = "M")

#knn
model_knn_df <- train(diagnosis ~., data = df_training, 
                      method = "knn", 
                      metric = "ROC", 
                      preProcess = c("scale", "center"), 
                      trControl = df_control, 
                      tuneLength =31)

plot(model_knn_df)

prediction_knn_df <- predict(model_knn_df, df_testing)
cm_knn_df <- confusionMatrix(prediction_knn_df, df_testing$diagnosis, positive = "M")
cm_knn_df

#svm
model_svm_df <- train(diagnosis ~., data = df_training, method = "svmRadial", 
                      metric = "ROC", 
                      preProcess = c("scale", "center"), 
                      trace = FALSE, 
                      trControl = df_control)

prediction_svm_df <- predict(model_svm_df, df_testing)
cm_svm_df <- confusionMatrix(prediction_svm_df, df_testing$diagnosis, positive = "M")
cm_svm_df

#neural network with lda
lda_training <- predict_lda_df[df_sampling_index, ]
lda_testing <- predict_lda_df[-df_sampling_index, ]
model_nnetlda_df <- train(diagnosis ~., lda_training, 
                          method = "nnet", 
                          metric = "ROC", 
                          preProcess = c("center", "scale"), 
                          tuneLength = 10, 
                          trace = FALSE, 
                          trControl = df_control)

prediction_nnetlda_df <- predict(model_nnetlda_df, lda_testing)
cm_nnetlda_df <- confusionMatrix(prediction_nnetlda_df, lda_testing$diagnosis, positive = "M")

#model evaluation
model_list <- list(rf = model_rf_df, svm = model_svm_df, 
                   logisic = model_logreg_df, Neural_with_LDA = model_nnetlda_df)
results <- resamples(model_list)

summary(results)


cm_list <- list(cm_rf = cm_rf_df, cm_svm = cm_svm_df, 
                cm_logisic = cm_logreg_df, cm_nnet_LDA = cm_nnetlda_df)
results <- map_df(cm_list, function(x) x$byClass) %>% as_tibble() %>% 
  mutate(stat = names(cm_rf_df$byClass))