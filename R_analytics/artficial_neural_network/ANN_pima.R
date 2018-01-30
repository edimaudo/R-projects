#remove old data
rm(list=ls())

#load data
pima_data <- read.table(file.choose(), sep = ",")

#load libraries
library(nnet)
library(ggplot2)
library(neuralnet)
library(tidyverse)


glimpse(pima_data)

#rename columns
names(pima_data) <- c('Number_of_times_pregnant','Plasma_glucose_concentration',
                      'Diastolic_blood_pressure','Triceps_skin_fold_thickness',
                      'Hour_serum_insulin','Body_mass_index','Diabetes_pedigree_function','Age','label')

plt1 <- ggplot(pima_data, aes(x = Body_mass_index, y = Age, colour = as.factor(label))) +
  geom_point(size=3) + ggtitle("BMI and Age")
plt2 <- ggplot(pima_data, aes(x = Diastolic_blood_pressure, y = Plasma_glucose_concentration, colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Plasma_glucose_concentration and Diastolic_blood_pressure")
plt1
plt2

# Encode as a one hot vector multilabel data
train <- pima_data[1:9]


# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[, 1:8] <- data.frame(lapply(train[, 1:8], scl))
head(train)



#fitting model using neuralnet
# Set up formula
n <- names(train)
f <- as.formula(paste("label ~", paste(n[!n %in% c("label")], collapse = " + ")))

set.seed(123)
nn <- neuralnet(f,
                data = train,
                hidden = c(3),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")

#show neural network
plot(nn, rep = 'best')

# Compute predictions
pr.nn <- compute(nn, train[, 1:8])

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)


# Set seed for reproducibility purposes

# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.95 # Set to 0.995 for LOOCV

# Crossvalidate, go!
for(i in 1:k)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_cv <- train[index, ]
  test_cv <- train[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(3),
                     act.fct = "logistic",
                     linear.output = FALSE)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:8])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[, 9])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)