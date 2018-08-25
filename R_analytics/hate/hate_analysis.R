#clear old data
rm(list=ls())

#load libraries
library(ggplot2)
library(corrplot)
library(tidyverse)

#load data
df <- read.csv(file.choose())

#View(df)

#visualize the distribution of raters from different countries
country_count <- df %>% 
  group_by(X_country, how_hateful) %>%
  summarise(count = n()) %>%
  select(X_country, how_hateful, count)
ggplot(data=country_count, aes(x= X_country, y=count, fill=how_hateful)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette="Paired") + theme_minimal() + 
  labs(x="Country", y="Count")

# -Which countries are most sensitive to hate? 
#(i.e., the share of 'very_hateful' *from all the ratings* (ratio) given by people from that country is the highest)
country_hate_count <- country_count %>%
  group_by(X_country) %>%
  summarize(total_hate = sum(count))

country_most_hate <- country_count %>%
  filter(how_hateful == "very_hateful") %>%
  select(X_country, count)

very_hateful_dist <- country_most_hate %>% 
  inner_join(country_hate_count, "X_country") %>%
  mutate(hate_ratio = count / total_hate) %>%
  arrange(desc(hate_ratio)) %>%
  select(X_country, hate_ratio)

# -Which countries are least sensitive to hate? 
#(i.e., the share of 'not_hateful_at_all' *from all the ratings* (ratio) given by people from that country is the highest)
country_least_hate <- country_count %>%
  filter(how_hateful == "not_hateful_at_all") %>%
  select(X_country, count)

least_hateful_dist <- country_least_hate %>% 
  inner_join(country_hate_count, "X_country") %>%
  mutate(least_hate_ratio = count / total_hate) %>%
  arrange(desc(least_hate_ratio)) %>%
  select(X_country, least_hate_ratio)



