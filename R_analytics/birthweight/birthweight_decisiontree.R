library(MASS)
library(rpart)
head(birthwt)

hist(birthwt$bwt)
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
birthwt[cols] <- lapply(birthwt[cols], as.factor)
set.seed(1)
train <- sample(1:nrow(birthwt), 0.75 * nrow(birthwt))
birthwtTree <- rpart(low ~ . - bwt, data = birthwt[train, ], method = 'class')
plot(birthwtTree)
text(birthwtTree, pretty = 0)

birthwtPred <- predict(birthwtTree, birthwt[-train, ], type = 'class')
table(birthwtPred, birthwt[-train, ]$low)