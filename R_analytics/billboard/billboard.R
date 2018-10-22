#bill board analysis

#clear old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot', 'data.table', 'lubridate',
              'stringr', "tidytext","tidyverse", "mlbench", "caret", "caTools")

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

#get summary
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

glimpse(df$Song)

#song mentions
song_mentions <- function(search_word, df){
  toMatch <-  c(search_word)
  count_val <- data.frame(string=df$Song, total_count=rowSums(sapply(toMatch, function(x) grepl(x, df$Song))))
  final_count <- count_val %>%
    count(total_count)
  return (final_count$n[2])
}

paste("Life shows up ", song_mentions("life",df), " times")
paste("war shows up ", song_mentions("war",df), " times")
paste("love shows up ", song_mentions("love",df), " times")

#enter a keyword and return get count of billboard songs with the keyword

billboard_songs <- function(search_word, df){
  search_word <- sapply(search_word, tolower)
  toMatch <-  c(search_word)
  count_val <- data.frame(string=df$Song, total_count=rowSums(sapply(toMatch, function(x) grepl(x, df$Song))))
  final_count <- count_val %>%
    count(total_count)
  output <- final_count$n[2]
  if (output == 0){
    return (paste("No songs were found to contain the word: ", search_word))
  } else if (output == 1) {
    return (paste(output ," song was found to have the phrase ' ", search_word, " ' in this dataset"))
  } else {
    return (paste(output ," songs were found to have the phrase ' ", search_word, " ' in this dataset"))
  }
}


billboard_songs("like", df)


#identify top words used in the top ten lyrics
df_top_ten <- df[1:10,]
df_top_ten_count <- data.frame(table(unlist(strsplit(tolower(df_top_ten$Lyrics), " "))))
df_top_20_word_count <- df_top_ten_count %>%
  arrange(desc(Freq)) %>%
  top_n(20)

