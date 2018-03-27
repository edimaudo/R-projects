#remove old data
rm(list=ls())
#load libraries
library(randomForest)
library(tidyverse)
library(caTools) # Data Splitting
library(caret) # Modeling toolkit
#load data
df <- read.csv(file.choose(), sep= ",")
#create backup
mydata.orig <- df

#######
# Clean 
#######

# Column Names to Lower Case
colnames(df) <- tolower(colnames(df))

# Select Columns for Model 
df <- df %>% select(name,
                    sex,
                    equipment, 
                    age, 
                    bodyweightkg, 
                    bestsquatkg, 
                    bestbenchkg, 
                    bestdeadliftkg,
                    totalkg, 
                    wilks)

# Clean feature names
df <- df %>% rename(bodyweight = bodyweightkg,
                    squat = bestsquatkg,
                    bench = bestbenchkg,
                    deadlift = bestdeadliftkg,
                    total = totalkg)

# Remove Incomplete Observations 
df <- df[complete.cases(df), ]

# Pre-processing: Split Data
set.seed(123)
split <- sample.split(df$sex, SplitRatio = 0.60)
df_training <- subset(df, split == TRUE)
df_test <- subset(df, split == FALSE)

#######
# Model
#######

model_random_forest <- randomForest(
  formula = sex ~ equipment + age + bodyweight + squat + bench + deadlift + total + wilks,
  data = df_training,
  importance = TRUE,
  ntree = 100
)

print(model_random_forest)

# Generate predicted classes using the model object
pred_model_decision_tree <- predict(object = model_random_forest, 
                                    newdata = df_test, 
                                    type="class")

# Calculate the confusion matrix for the test set
confusionMatrix(data = pred_model_decision_tree, 
                reference = df_test$sex)


# Predict using the test set
prediction <- predict(model_random_forest, df_test, type = "prob")

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(name = df_test$name, sex = prediction)

# Write the solution to file
#write.csv(solution, file = 'model_random_forest_predictioms.csv')
head(solution)
