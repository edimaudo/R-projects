#more data exploration using decision tress
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')

#load data
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

#decision tree library
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

plot(fit)
text(fit)
rpart.Plot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
