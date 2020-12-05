# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','performanceEstimation')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose(),sep = ",")

#check columns
unique(df$Category)
unique(df$Procuct)
unique(df$Country)
unique(df$Date)
unique(df$Year)
unique(df$Verified)

#select only needed columns
df <- df %>%
  select(Procuct,Country,Month,Year, Verified, Helpful,Title, Body, Rating) %>%
  na.omit()

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

New_df <- df %>%
  select(Body, Rating)

#split into train and test
set.seed(123)
sample <- sample.split(New_df,SplitRatio = 0.75)
train <- subset(New_df,sample ==TRUE)
test <- subset(New_df, sample==FALSE)

clean_data <- function(df){
  
}

test_corpus <- VCorpus(VectorSource(test_dat$transcript))
##Removing Punctuation
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))
##Removing numbers
test_corpus <- tm_map(test_corpus, removeNumbers)
##Converting to lower case
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
##Removing stop words
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords(“english”))
##Stemming
test_corpus <- tm_map(test_corpus, stemDocument)
##Whitespace
test_corpus <- tm_map(test_corpus, stripWhitespace)
# Create Document Term Matrix
dtm_test <- DocumentTermMatrix(test_corpus)
test_corpus <- removeSparseTerms(dtm_test, 0.4)
dtm_test_matrix <- as.matrix(test_corpus)



dtm_train_matrix <- cbind(dtm_train_matrix, train_dat$highest_rating)
colnames(dtm_train_matrix)[ncol(dtm_train_matrix)] <- “y”
training_set_ted_talk <- as.data.frame(dtm_train_matrix)
training_set_ted_talk$y <- as.factor(training_set_ted_talk$y)


review_ted_model <- train(y ~., data = training_set_ted_talk, method = ‘svmLinear3’)

#Build the prediction 
model_ted_talk_result <- predict(review_ted_model, newdata = dtm_test_matrix)
check_accuracy <- as.data.frame(cbind(prediction = model_ted_talk_result, rating = test_dat$highest_rating))

check_accuracy <- check_accuracy %>% mutate(prediction = as.integer(prediction) — 1)
check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)
round(prop.table(table(check_accuracy$accuracy)), 3)

classificationMetrics(as.integer(test_dat$highest_rating), model_ted_talk_result)
most_common_misclassified_ratings = check_accuracy %>% filter(check_accuracy$accuracy == 0) %>%
  group_by(rating) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(3)
##Most commong missclassified rating
levels(train_dat$highest_rating)[most_common_misclassified_ratings$rating]