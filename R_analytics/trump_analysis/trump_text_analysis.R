#remove old data
rm(list=ls())
#packages
packages <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
              "cluster", "igraph", "fpc")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
docs <- VCorpus(DirSource(getwd()))   
summary(docs)

inspect(docs[1])

#preprocessing
docs <- tm_map(docs,removePunctuation) 

#remove special characters

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character that did not translate, so it had to be removed.
}

docs <- tm_map(docs, removeNumbers) #remove numbers

#convert to lowercase
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

# remove stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)

#remove particular words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))  

#combining words 
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) # Check to see if it worked.

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1]))

#remove whitespace
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)

#document term matrix
dtm <- DocumentTermMatrix(docs)   
dtm

tdm <- TermDocumentMatrix(docs)   
tdm

freq <- colSums(as.matrix(dtm))   
length(freq)   

ord <- order(freq) 

m <- as.matrix(dtm)   
dim(m)

#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms

#word frequency
freq <- colSums(as.matrix(dtm))

head(table(freq), 20)
tail(table(freq), 20)

freq <- colSums(as.matrix(dtms))   
freq 

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

#correlations
findAssocs(dtm, c("country" , "american"), corlimit=0.85)

#word cloud
library(RColorBrewer)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)  

#clustering by term similarity
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
dtmss

#hierarchy clustering
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")   # for a different look try substituting: method="ward.D"
fit

#k means clustering
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)