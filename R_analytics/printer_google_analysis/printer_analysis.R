rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",'readxl','tidyr',
              'scales','dplyr','wordcloud2','gridExtra',
              'tidytext','stringr','reshape2', 
              'textmineR','topicmodels','textclean','pals','lubridate')
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
# Text analytics
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

remove_keywords <- c("printer","print", "printing","app")

# word breakdown 
review_words <- df %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3,!word %in% remove_keywords) 

# word frequency
full_word_count <- df %>%
  unnest_tokens(word, Review) %>%
  group_by(Product,Rating) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

write.csv(full_word_count,"product_rating.csv")

# word count visualization
full_word_count %>%
  ggplot(aes(x = Rating,y=num_words, fill = Product )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  ylab("Word Count") + 
  xlab("Rating") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# top 100 words used
top_100_words <- review_words %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

write.csv(top_100_words, "top_100_words.csv")

# top 20 words across all reviews and brands visualization
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

# keywords by Product and ratings
review_product_rating <- review_words %>%
  group_by(Product, Rating) %>%
  count(word, sort = TRUE) %>%
  select(Product, Rating, word, n) %>%
  arrange(desc(Product,Rating))  

write.csv(review_product_rating, "review_product_rating.csv")

review_product <- review_words %>%
  group_by(Product) %>%
  count(word, sort = TRUE) %>%
  select(Product, word, n) %>%
  arrange(desc(Product)) 

write.csv(review_product, "review_product.csv")

review_rating <- review_words %>%
  group_by(Rating) %>%
  count(word, sort = TRUE) %>%
  select(Rating, word, n) %>%
  arrange(desc(Rating)) 

write.csv(review_rating, "review_rating.csv")

# word cloud
words_counts <- review_words %>%
  count(word, sort = TRUE) 

wordcloud2(words_counts[1:100, ], size = 1)

#=============
# TF-IDF analysis
#=============

# tf-idf by Product & Rating
popular_tfidf_words <- df %>%
  unnest_tokens(word, Review) %>%
  distinct() %>%
  filter(nchar(word) > 3, !word %in% remove_keywords) %>%
  count(Product, Rating, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, Rating, n)

top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Product, Rating) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(desc(Product, Rating)) %>%
  mutate(row = row_number())

#td-idf by Product
top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = Product)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Product") +
  theme_bw() +  
  facet_wrap(~Product, ncol = 4, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()


write.csv(top_popular_tfidf_words, "top_popular_tfidf_words.csv")



# =================
# Topic modelling
#===================

data_1 <- df %>% filter(Rating == 1)
data_2 <- df %>% filter(Rating  == 2)
data_3 <- df %>% filter(Rating  == 3)
data_4 <- df %>% filter(Rating  == 4)
data_5 <- df %>% filter(Rating  == 5)
table(df$Rating)

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
  
  return(as.data.frame(x))

}

set.seed(1502)

# Rating 5
# apply textcleaner function. note: we only clean the text without convert it to dtm
clean_5 <- textcleaner(data_5$Review)
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
# mod_lda_5$linguistic <- CalcHellingerDist(mod_lda_5$phi)
# mod_lda_5$hclust <- hclust(as.dist(mod_lda_5$linguistic),"ward.D")
# mod_lda_5$hclust$labels <- paste(mod_lda_5$hclust$labels, mod_lda_5$labels[,1])
# plot(mod_lda_5$hclust)

#modsum_5 %>% 
#  arrange(desc(coherence)) %>%
#  slice(1:5)

top_terms_5 <- data.frame(mod_lda_5$top_terms)

# Rating 4
clean_4 <- textcleaner(data_4$Review)
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
# mod_lda_4$linguistic <- CalcHellingerDist(mod_lda_4$phi)
# mod_lda_4$hclust <- hclust(as.dist(mod_lda_4$linguistic),"ward.D")
# mod_lda_4$hclust$labels <- paste(mod_lda_4$hclust$labels, mod_lda_4$labels[,1])
# plot(mod_lda_4$hclust)

# modsum_4 %>% 
#   arrange(desc(coherence)) %>%
#   slice(1:5)

top_terms_4 <- data.frame(mod_lda_4$top_terms)


# Rating 3
clean_3 <- textcleaner(data_3$Review)
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
# mod_lda_3$linguistic <- CalcHellingerDist(mod_lda_3$phi)
# mod_lda_3$hclust <- hclust(as.dist(mod_lda_3$linguistic),"ward.D")
# mod_lda_3$hclust$labels <- paste(mod_lda_3$hclust$labels, mod_lda_3$labels[,1])
# plot(mod_lda_3$hclust)
# 
# modsum_3 %>% 
#   arrange(desc(coherence)) %>%
#   slice(1:5)

top_terms_3 <- data.frame(mod_lda_3$top_terms)

# Rating 2
clean_2 <- textcleaner(data_2$Review)
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
# mod_lda_2$linguistic <- CalcHellingerDist(mod_lda_2$phi)
# mod_lda_2$hclust <- hclust(as.dist(mod_lda_2$linguistic),"ward.D")
# mod_lda_2$hclust$labels <- paste(mod_lda_2$hclust$labels, mod_lda_2$labels[,1])
# plot(mod_lda_2$hclust)
# 
# modsum_2 %>% 
#   arrange(desc(coherence)) %>%
#   slice(1:5)

top_terms_2 <- data.frame(mod_lda_2$top_terms)

# Rating 1
clean_1 <- textcleaner(data_1$Review)
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
# mod_lda_1$linguistic <- CalcHellingerDist(mod_lda_1$phi)
# mod_lda_1$hclust <- hclust(as.dist(mod_lda_1$linguistic),"ward.D")
# mod_lda_1$hclust$labels <- paste(mod_lda_1$hclust$labels, mod_lda_1$labels[,1])
# plot(mod_lda_1$hclust)
# 
# modsum_1 %>% 
#   arrange(desc(coherence)) %>%
#   slice(1:5)

top_terms_1 <- data.frame(mod_lda_1$top_terms)

# Topics