#clear environment
rm(list = ls())

#packages 
packages <- c('tidyverse','dplyr', 'readxl', 'ggplot2')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel("Customer data.xlsx")

df_agg <- df %>%
  group_by(`Customer name`) %>%
  summarise(total = sum(`Sales (AUD)`)) %>%
  select(`Customer name`,total) %>%
  arrange(desc(total))

df_agg <- na.omit(df_agg)


ggplot(df_agg, aes(x = reorder(`Customer name`, -total),y=total)) + 
  geom_bar(stat = "identity",width = 0.5, fill="steelblue") + theme_classic() + 
  labs(x = "Customer Name", y = "Total Sales (AUD)") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))
