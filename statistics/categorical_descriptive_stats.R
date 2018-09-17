#clear old data
rm(list=ls())

#libraries
library(readxl)
library(ggplot2)
library(tidyverse)

#read data
supermarket <- read_excel(file.choose(), sheet = 2) #supermarket data

#contigency table
table(supermarket$Gender)

# cross classication counts for gender by marital status
table(supermarket$`Marital Status`, supermarket$Gender)

# customer counts across location by gender and marital status
table1 <- table(supermarket$`Marital Status`, supermarket$Gender, 
                supermarket$`State or Province`)
ftable(table1)

#proportions
# percentages of gender categories
table2 <- table(supermarket$Gender)
prop.table(table2)

#visualization
ggplot(supermarket, aes(x = `State or Province`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(supermarket, aes(x = reorder_size(`State or Province`))) +
  geom_bar() +
  xlab("State or Province") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Proportions bar charts
ggplot(supermarket, aes(x = reorder_size(`State or Province`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("State or Province") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))