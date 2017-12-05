library("readxl")

powerData <- read_excel(file.choose())

head(powerData)

set.seed(123)
split <- sample(nrow(powerData), size = floor(0.75 * nrow(powerData)))
trainData <- powerData[split, ]
testData <- powerData[-split, ]
head(trainData)
head(testData)

predictionModel <- lm(PE ~ ., data = trainData)
summary(predictionModel)

#testing
prediction <- predict(predictionModel, newdata = testData)