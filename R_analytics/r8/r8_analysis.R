library(tm)
library(wordcloud)
library(Rgraphviz)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(plyr)
library(ggplot2)
library(RTextTools)
library(e1071)

x <- read.table('r8-train-all-terms.txt', header=FALSE, sep='\t')


source <- VectorSource(x$V2)
corpus <- Corpus(source)

#data prep
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#clustering
mat <- DocumentTermMatrix(corpus)
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)
norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
mat_norm <- norm_eucl(mat4)

set.seed(5)
k <- 3
kmeansResult <- kmeans(mat_norm, k)

kmeansResult$cluster[1:5]
count(kmeansResult$cluster)

#testing k means
result <- data.frame('actual'=x$V1, 'predicted'=kmeansResult$cluster)
result <- result[order(result[,1]),]

result$counter <- 1
result.agg <- aggregate(counter~actual+predicted, data=result, FUN='sum')

result.agg

ggplot(data=result.agg, aes(x=actual, y=predicted, size=counter)) + geom_point()


#topic modeling
k <- 3
lda <- LDA(mat, k)
terms(lda)

x <- topics(lda)
new.df <- data.frame('response'=names(x), 'topic'=x, row.names=NULL)
count(new.df, vars='topic')

#supervised learning
set.seed(10)
x <- read.table('r8-train-all-terms.txt', header=FALSE, sep='\t')
x.rand <- x[sample(1:nrow(x)),]
x.rand <- x.rand[which(x.rand$V1 %in% c('trade','crude', 'money-fx')),]
source <- VectorSource(x.rand$V2)
corpus <- Corpus(source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

mat <- DocumentTermMatrix(corpus)

mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

classifier <- naiveBayes(mat4[1:568,], x.rand$V1[1:568])
predicted <- predict(classifier, mat4[569:710,])
table(as.character(x.rand$V1[569:710]), as.character(predicted))

recall_accuracy(as.character(x.rand$V1[569:710]), as.character(predicted))


container <- create_container(mat, x.rand$V1, trainSize=1:568,testSize=569:710, virgin=FALSE)
model <- train_model(container, 'TREE',kernel='linear')
results <- classify_model(container, model)
table(as.character(x.rand$V1[569:710]), as.character(results[,"TREE_LABEL"]))

recall_accuracy(x.rand$V1[569:710], results[,"TREE_LABEL"])

#svm model
container <- create_container(mat, x.rand$V1, trainSize=1:568,testSize=569:710, virgin=FALSE)
model <- train_model(container, 'SVM',kernel='linear')
results <- classify_model(container, model)

ecall_accuracy(x.rand$V1[569:710], results[,"SVM_LABEL"])
table(as.character(x.rand$V1[569:710]), as.character(results[,"SVM_LABEL"]))

recall_accuracy(x.rand$V1[569:710], results[,"SVM_LABEL"])
