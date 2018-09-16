library(rvest)
webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
results <- webpage %>% html_nodes(".short-desc")

first_result <- results[1]
first_result %>% html_nodes("strong")


first_result <- results[1]
date <- first_result %>% html_nodes("strong") %>% html_text(trim = TRUE)

library(stringr)
str_c(date, ', 2017')

#lie
xml_contents(first_result)
lie <- xml_contents(first_result)[2] %>% html_text(trim = TRUE)
str_sub(lie, 2, -2)

explanation <- first_result %>% html_node(".short-truth") %>% html_text(trim = TRUE)
str_sub(explanation, 2, -2)

url <- first_result %>% html_node("a") %>% html_attr("href")

#build lie data
library(dplyr)
records <- vector("list", length = length(results))

for (i in seq_along(results)) {
  date <- str_c(results[i] %>% html_nodes("strong") %>% html_text(trim = TRUE), ", 2017")
  lie <- str_sub(xml_contents(results[i])[2] %>% html_text(trim = TRUE), 2, -2)
  explanation <- str_sub(results[i] %>% html_nodes(".short-truth") %>% html_text(trim = TRUE), 2, -2)
  url <- results[i] %>% html_nodes("a") %>% html_attr("href")
  records[[i]] <- data_frame(date = date, lie = lie, explanation = explanation, url = url)
}

df <- bind_rows(records)
glimpse(df)

library(lubridate)
df$date <- mdy(df$date)
glimpse(df)