#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','readxl',
              'cluster','factoextra','psy','lattice','nFactors','scales','NbClust','keras')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

glimpse(df)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

df.backup <- df


#scale data
df[,c(1,2,3,4,5,6,7,8)] <- scale(df[,c(1,2,3,4,5,6,7,8)])

#set y as factor
df$y <- as.factor(df$y)

#split in train and test data
set.seed(123)
sample <- sample.split(df,SplitRatio = 0.70)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

X_train <- train[,c(1,2,3,4,5,6,7,8)]
y_train <- train$y
X_test <- test[,c(1,2,3,4,5,6,7,8)]
y_test <- test$y


#build model 
model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 100, 
  batch_size = 5,
  validation_split = 0.3
)