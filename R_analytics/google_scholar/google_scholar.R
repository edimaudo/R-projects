library(rvest)
library(ggplot2)

page <- read_html("https://scholar.google.com/citations?user=sTR9SIQAAAAJ&hl=en&oi=ao")

citations <- page %>% html_nodes ("#gsc_a_b .gsc_a_c") %>% html_text()%>%as.numeric()

barplot(citations, main="How many times has each paper been cited?", ylab='Number of citations', col="skyblue", xlab="")

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
Coauthors = page%>% html_nodes(css=".gsc_1usr_name a") %>% html_text()
Coauthors = as.data.frame(Coauthors)
names(Coauthors)='Coauthors'

# of citations
page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
citations = page%>% html_nodes(css = ".gsc_1usr_cby")%>%html_text()
citations 
