data <- read.csv('regression.csv', sep=",", header = TRUE)

# Plot the data
plot(data, pch=16)

# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
abline(model)

plot(data, pch=16)
model <- lm(Y ~ X , data)

# make a prediction for each X
predictedY <- predict(model, data)

# display the predictions
points(data$X, predictedY, col = "blue", pch=4)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)

library(e1071)
model <- svm(Y ~ X , data)

predictedY <- predict(model, data)

points(data$X, predictedY, col = "red", pch=4)

error <- data$Y - predictedY
svrPredictionRMSE <- rmse(error) 


#model tuning
# perform a grid search
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

#refine model and focus on darker region
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)

#select model
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

error <- data$Y - tunedModelY  

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error) 