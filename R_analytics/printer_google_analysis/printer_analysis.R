rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",'readxl','tidyr',
              'scales','dplyr','mlbench','caTools','wordcloud2','gridExtra',
              'tidytext','stringr','reshape2',"tm", "SnowballCC", "RColorBrewer", 
              'textmineR','topicmodels','textclean','pals',"biclust", "cluster",
              "igraph","fpc",'lubridate')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#=============
# Load data
#=============
df <- read_excel(file.choose())

df.backup <- df #backup

#df$Product <- ifelse(df$appId == "com.hp.printercontrol", 'HP',
#                     ifelse(df$appId == "jp.co.canon.bsd.ad.pixmaprint", 'Canon',
#                            ifelse(df$appId == "epson.print", 'Epson', 'Epson-Smart')))

#=============
# Text analysis
#=============
# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# fix (expand) contractions
df$Review <- sapply(df$Review, fix.contractions)

# remove special characters
df$Review <- sapply(df$Review, removeSpecialChars)

# convert everything to lower case
df$Review <- sapply(df$Review, tolower)

#=============
# Text mining
#=============

# word breakdown 
review_words <- df %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3,!word %in% c("printer","print", "printing")) #remove printer and print

# word frequency
full_word_count <- df %>%
  unnest_tokens(word, Review) %>%
  group_by(Brand,Rating) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

write.csv(full_word_count,"brand_rating.csv")

# word count visualization
full_word_count %>%
  ggplot(aes(x = Rating,y=num_words, fill = Brand )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  ylab("Word Count") + 
  xlab("Rating") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())


# top 20 words across all reviews and brands
review_words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Word Count") +
  ggtitle("Top 20 Most Frequently Used Words") +
  coord_flip()

# keywords by Brand and ratings
review_brand_rating <- review_words %>%
  group_by(Brand, Rating) %>%
  count(word, sort = TRUE) %>%
  select(Brand, Rating, word, n) %>%
  arrange(desc(Brand,Rating))  

# word cloud
words_counts <- review_words %>%
  count(word, sort = TRUE) 

wordcloud2(words_counts[1:100, ], size = .5)

# tf-idf
popular_tfidf_words <- df %>%
  unnest_tokens(word, Review) %>%
  distinct() %>%
  filter(nchar(word) > 3, !word %in% c("printer","print", "printing")) %>%
  count(Brand, Rating, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, Rating, n)

top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Brand, Rating) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Brand, Rating, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = Brand)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Brand") +
  theme_bw() +  
  facet_wrap(~Brand, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()

#=============
# Topic modeling
#=============
# build textcleaner function
textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en"))
  
  # convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
  
}

#remove printer and print words
data_1 <- df %>% filter(Rating == 1)
data_2 <- df %>% filter(Rating  == 2)
data_3 <- df %>% filter(Rating  == 3)
data_4 <- df %>% filter(Rating  == 4)
data_5 <- df %>% filter(Rating  == 5)
table(df$Rating)

# apply textcleaner function for review text
dtm_5 <- textcleaner(data_5$Review)
# find most frequent terms. i choose words that at least appear in 50 reviews
freqterm_5 <- findFreqTerms(dtm_5,50)
# we have 981 words. subset the dtm to only choose those selected words
dtm_5 <- dtm_5[,freqterm_5]
# only choose words that appear once in each rows
rownum_5 <- apply(dtm_5,1,sum)
dtm_5 <- dtm_5[rownum_5>0,]
# apply to LDA function. set the k = 6, means we want to build 6 topic 
lda_5 <- LDA(dtm_5,k = 6,control = list(seed = 1502))
# apply auto tidy using tidy and use beta as per-topic-per-word probabilities
topic_5 <- tidy(lda_5,matrix = "beta")
# choose 15 words with highest beta from each topic
top_terms_5 <- topic_5 %>%
  filter(!term %in%  c("printer","print", "printing")) %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
# plot the topic and words for easy interpretation
plot_topic_5 <- top_terms_5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_5


# rating 4
dtm_4 <- textcleaner(data_4$Review)
freqterm_4 <- findFreqTerms(dtm_4,20)
dtm_4 <- dtm_4[,freqterm_4]
rownum_4 <- apply(dtm_4,1,sum)
dtm_4 <- dtm_4[rownum_4>0,]
lda_4 <- LDA(dtm_4,k = 6,control = list(seed = 1502))
topic_4 <- tidy(lda_4,matrix = "beta")

top_terms_4 <- topic_4 %>%
  filter(!term %in%  c("printer","print", "printing")) %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_4 <- top_terms_4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_4

# rating 3
dtm_3 <- textcleaner(data_3$Review)
freqterm_3 <- findFreqTerms(dtm_3,10)
dtm_3 <- dtm_3[,freqterm_3]
rownum_3 <- apply(dtm_3,1,sum)
dtm_3 <- dtm_3[rownum_3>0,]
lda_3 <- LDA(dtm_3,k = 6,control = list(seed = 1502))
topic_3 <- tidy(lda_3,matrix = "beta")
top_terms_3 <- topic_3 %>%
  filter(!term %in%  c("printer","print", "printing")) %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_3 <- top_terms_3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_3

# rating 2
dtm_2 <- textcleaner(data_2$Review)
freqterm_2 <- findFreqTerms(dtm_2,5)
dtm_2 <- dtm_2[,freqterm_2]
rownum_2 <- apply(dtm_2,1,sum)
dtm_2 <- dtm_2[rownum_2>0,]
lda_2 <- LDA(dtm_2,k = 6,control = list(seed = 1502))
topic_2 <- tidy(lda_2,matrix = "beta")
top_terms_2 <- topic_2 %>%
  filter(!term %in%  c("printer","print", "printing")) %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_2 <- top_terms_2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_2

# rating 1
dtm_1 <- textcleaner(data_1$Review)
freqterm_1 <- findFreqTerms(dtm_1,5)
dtm_1 <- dtm_1[,freqterm_1]
rownum_1 <- apply(dtm_1,1,sum)
dtm_1 <- dtm_1[rownum_1>0,]
lda_1 <- LDA(dtm_1,k = 6,control = list(seed = 1502))
topic_1 <- tidy(lda_1,matrix = "beta")
top_terms_1 <- topic_1 %>%
  filter(!term %in%  c("printer","print", "printing")) %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_1 <- top_terms_1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_1

#===================
# Topic modelliing 2
#===================

textcleaner_2 <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  return(as.data.frame(x))

}

set.seed(1502)

# Rating 5
# apply textcleaner function. note: we only clean the text without convert it to dtm
clean_5 <- textcleaner_2(data_5$Review)
clean_5 <- clean_5 %>% mutate(id = rownames(clean_5))

# crete dtm
dtm_r_5 <- CreateDtm(doc_vec = clean_5$x,
                     doc_names = clean_5$id,
                     ngram_window = c(1,2),
                     stopword_vec = stopwords("en"),
                     verbose = F)

dtm_r_5 <- dtm_r_5[,colSums(dtm_r_5)>2]

mod_lda_5 <- FitLdaModel(dtm = dtm_r_5,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_5$top_terms <- GetTopTerms(phi = mod_lda_5$phi,M = 15)
mod_lda_5$prevalence <- colSums(mod_lda_5$theta)/sum(mod_lda_5$theta)*100

mod_lda_5$summary <- data.frame(topic = rownames(mod_lda_5$phi),
                                coherence = round(mod_lda_5$coherence,3),
                                prevalence = round(mod_lda_5$prevalence,3),
                                top_terms = apply(mod_lda_5$top_terms,2,
                                                  function(x){paste(x,collapse = ", ")}))

modsum_5 <- mod_lda_5$summary %>%
  `rownames<-`(NULL)

#visualization
modsum_5 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Text review with 5 rating",
       x = "Topics", y = "Value")

# denodogram clustering
mod_lda_5$linguistic <- CalcHellingerDist(mod_lda_5$phi)
mod_lda_5$hclust <- hclust(as.dist(mod_lda_5$linguistic),"ward.D")
mod_lda_5$hclust$labels <- paste(mod_lda_5$hclust$labels, mod_lda_5$labels[,1])
plot(mod_lda_5$hclust)

modsum_5 %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

data.frame(mod_lda_5$top_terms)

# Rating 4
clean_4 <- textcleaner_2(data_4$Review)
clean_4 <- clean_4 %>% mutate(id = rownames(clean_4))

# crete dtm
dtm_r_4 <- CreateDtm(doc_vec = clean_4$x,
                     doc_names = clean_4$id,
                     ngram_window = c(1,2),
                     stopword_vec = stopwords("en"),
                     verbose = F)

dtm_r_4 <- dtm_r_4[,colSums(dtm_r_4)>2]

mod_lda_4 <- FitLdaModel(dtm = dtm_r_4,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_4$top_terms <- GetTopTerms(phi = mod_lda_4$phi,M = 15)
mod_lda_4$prevalence <- colSums(mod_lda_4$theta)/sum(mod_lda_4$theta)*100

mod_lda_4$summary <- data.frame(topic = rownames(mod_lda_4$phi),
                                coherence = round(mod_lda_4$coherence,3),
                                prevalence = round(mod_lda_4$prevalence,3),
                                top_terms = apply(mod_lda_4$top_terms,2,
                                                  function(x){paste(x,collapse = ", ")}))

modsum_4 <- mod_lda_4$summary %>%
  `rownames<-`(NULL)

#visualization
modsum_4 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Text review with 4 rating",
       x = "Topics", y = "Value")

# denodogram clustering
mod_lda_4$linguistic <- CalcHellingerDist(mod_lda_4$phi)
mod_lda_4$hclust <- hclust(as.dist(mod_lda_4$linguistic),"ward.D")
mod_lda_4$hclust$labels <- paste(mod_lda_4$hclust$labels, mod_lda_4$labels[,1])
plot(mod_lda_4$hclust)

modsum_4 %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

data.frame(mod_lda_4$top_terms)


# Rating 3
clean_3 <- textcleaner_2(data_3$Review)
clean_3 <- clean_3 %>% mutate(id = rownames(clean_3))

# crete dtm

dtm_r_3 <- CreateDtm(doc_vec = clean_3$x,
                     doc_names = clean_3$id,
                     ngram_window = c(1,2),
                     stopword_vec = stopwords("en"),
                     verbose = F)

dtm_r_3 <- dtm_r_3[,colSums(dtm_r_3)>2]

mod_lda_3 <- FitLdaModel(dtm = dtm_r_3,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_3$top_terms <- GetTopTerms(phi = mod_lda_3$phi,M = 15)
mod_lda_3$prevalence <- colSums(mod_lda_3$theta)/sum(mod_lda_3$theta)*100

mod_lda_3$summary <- data.frame(topic = rownames(mod_lda_3$phi),
                                coherence = round(mod_lda_3$coherence,3),
                                prevalence = round(mod_lda_3$prevalence,3),
                                top_terms = apply(mod_lda_3$top_terms,2,
                                                  function(x){paste(x,collapse = ", ")}))

modsum_3 <- mod_lda_3$summary %>%
  `rownames<-`(NULL)

#visualization
modsum_3 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Text review with 3 rating",
       x = "Topics", y = "Value")

# denodogram clustering
mod_lda_3$linguistic <- CalcHellingerDist(mod_lda_3$phi)
mod_lda_3$hclust <- hclust(as.dist(mod_lda_3$linguistic),"ward.D")
mod_lda_3$hclust$labels <- paste(mod_lda_3$hclust$labels, mod_lda_3$labels[,1])
plot(mod_lda_3$hclust)

modsum_3 %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

data.frame(mod_lda_3$top_terms)

# Rating 2
clean_2 <- textcleaner_2(data_2$Review)
clean_2 <- clean_2 %>% mutate(id = rownames(clean_2))

# crete dtm
dtm_r_2 <- CreateDtm(doc_vec = clean_2$x,
                     doc_names = clean_2$id,
                     ngram_window = c(1,2),
                     stopword_vec = stopwords("en"),
                     verbose = F)

dtm_r_2 <- dtm_r_2[,colSums(dtm_r_2)>2]

mod_lda_2 <- FitLdaModel(dtm = dtm_r_2,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_2$top_terms <- GetTopTerms(phi = mod_lda_2$phi,M = 15)
mod_lda_2$prevalence <- colSums(mod_lda_2$theta)/sum(mod_lda_2$theta)*100

mod_lda_2$summary <- data.frame(topic = rownames(mod_lda_2$phi),
                                coherence = round(mod_lda_2$coherence,3),
                                prevalence = round(mod_lda_2$prevalence,3),
                                top_terms = apply(mod_lda_2$top_terms,2,
                                                  function(x){paste(x,collapse = ", ")}))

modsum_2 <- mod_lda_2$summary %>%
  `rownames<-`(NULL)

#visualization
modsum_2 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Text review with 2 rating",
       x = "Topics", y = "Value")

# denodogram clustering
mod_lda_2$linguistic <- CalcHellingerDist(mod_lda_2$phi)
mod_lda_2$hclust <- hclust(as.dist(mod_lda_2$linguistic),"ward.D")
mod_lda_2$hclust$labels <- paste(mod_lda_2$hclust$labels, mod_lda_2$labels[,1])
plot(mod_lda_2$hclust)

modsum_2 %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

data.frame(mod_lda_2$top_terms)

# Rating 1
clean_1 <- textcleaner_2(data_1$Review)
clean_1 <- clean_1 %>% mutate(id = rownames(clean_1))

# crete dtm
dtm_r_1 <- CreateDtm(doc_vec = clean_1$x,
                     doc_names = clean_1$id,
                     ngram_window = c(1,2),
                     stopword_vec = stopwords("en"),
                     verbose = F)

dtm_r_1 <- dtm_r_1[,colSums(dtm_r_1)>2]

mod_lda_1 <- FitLdaModel(dtm = dtm_r_1,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_1$top_terms <- GetTopTerms(phi = mod_lda_1$phi,M = 15)
mod_lda_1$prevalence <- colSums(mod_lda_1$theta)/sum(mod_lda_1$theta)*100

mod_lda_1$summary <- data.frame(topic = rownames(mod_lda_1$phi),
                                coherence = round(mod_lda_1$coherence,3),
                                prevalence = round(mod_lda_1$prevalence,3),
                                top_terms = apply(mod_lda_1$top_terms,2,
                                                  function(x){paste(x,collapse = ", ")}))

modsum_1 <- mod_lda_1$summary %>%
  `rownames<-`(NULL)

#visualization
modsum_1 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "Text review with 5 rating",
       x = "Topics", y = "Value")

# denodogram clustering
mod_lda_1$linguistic <- CalcHellingerDist(mod_lda_1$phi)
mod_lda_1$hclust <- hclust(as.dist(mod_lda_1$linguistic),"ward.D")
mod_lda_1$hclust$labels <- paste(mod_lda_1$hclust$labels, mod_lda_1$labels[,1])
plot(mod_lda_1$hclust)

modsum_1 %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

data.frame(mod_lda_1$top_terms)