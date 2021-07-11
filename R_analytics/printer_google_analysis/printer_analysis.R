rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",'readxl','tidyr',
              'scales','dplyr','mlbench','caTools','wordcloud2','gridExtra',
              'tidytext','stringr','reshape2',"tm", "SnowballCC", "RColorBrewer", 
              'topicmodels','pals',"biclust", "cluster", "igraph", "fpc",'forecast',
              'TTR','xts','lubridate')
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

glimpse(df)

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
  filter(nchar(word) > 3)

# word frequency
full_word_count <- df %>%
  unnest_tokens(word, Review) %>%
  group_by(Brand,Rating) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

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


#top words across all reviews and brands
review_words %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words") +
  coord_flip()

#=============
# Topic modeling
#=============
