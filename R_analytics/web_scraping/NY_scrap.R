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