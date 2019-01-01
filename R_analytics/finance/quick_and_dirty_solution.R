library(tree)
library(AUC)

X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)

y_train <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])

tree_classifier <- tree(y ~ ., data = cbind(X_train, y = y_train))
training_scores <- predict(tree_classifier, X_train)
roc_curve <- roc(predictions = training_scores[,2], labels = y_train)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

test_scores <- predict(tree_classifier, X_test)
write.table(test_scores[,2], file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)
