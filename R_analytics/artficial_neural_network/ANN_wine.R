#remove old data
rm(list=ls())

#load data
wine_data <- read.table(file.choose(), sep = ",")

glimpse(wine_data)

#rename columns
names(wine_data) <- c("label",'Alcohol','Malic_acid','Ash','Alcalinity_of_ash','Magnesium','Total_phenols','Flavanoids','Nonflavanoid_phenols','Proanthocyanins','Color_intensity','Hue','OD280_OD315_of_diluted_wines','Proline')

glimpse(wine_data)

#multi-label classification problem

#load libraries
library(nnet)
library(ggplot2)
library(neuralnet)
library(tidyverse)

plt1 <- ggplot(wine_data, aes(x = Alcohol, y = Magnesium, colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Wines")
plt2 <- ggplot(wine_data, aes(x = Alcohol, y = Proline, colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Wines")
plt1
plt2


# Encode as a one hot vector multilabel data
train <- cbind(wine_data[, 2:14], class.ind(as.factor(wine_data$label)))
# Set labels name
names(train) <- c(names(wine_data)[2:14],"l1","l2","l3")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[, 1:13] <- data.frame(lapply(train[, 1:13], scl))
head(train)

#fitting model using neuralnet
# Set up formula
n <- names(train)
f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1","l2","l3")], collapse = " + ")))

set.seed(123)
nn <- neuralnet(f,
                data = train,
                hidden = c(13, 10, 3),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")

#show neural network
plot(nn, rep = 'best')

# Compute predictions
pr.nn <- compute(nn, train[, 1:13])

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
                     hidden = c(13, 10, 3),
                     act.fct = "logistic",
                     linear.output = FALSE)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:13])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[, 14:16])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)