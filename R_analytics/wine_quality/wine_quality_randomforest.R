url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine <- read.table(url, sep = ';', header = TRUE)
head(wine)
barplot(table(wine$quality))

wine$taste <- ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] <- 'normal'
wine$taste <- as.factor(wine$taste)
table(wine$taste)

#split data
set.seed(123)
samp <- sample(nrow(wine), 0.6 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

#forest model
library(randomForest)
model <- randomForest(taste ~ . - quality, data = train)

pred <- predict(model, newdata = test)
table(pred, test$taste)