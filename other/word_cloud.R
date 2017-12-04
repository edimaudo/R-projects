library(tm)
library(SnowballC)
library(wordcloud)

jeopQ <- read.csv('JEOPARDY_CSV.csv', stringsAsFactors = FALSE)

jeopCorpus <- Corpus(VectorSource(jeopQ$Question))

jeopCorpus <- tm_map(jeopCorpus, content_transformer(tolower))

jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))

jeopCorpus <- tm_map(jeopCorpus, stemDocument)

wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)