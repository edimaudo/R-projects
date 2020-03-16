#remove old data
rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','proxy','reshape2','recommenderlab')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

users <- read_csv("BX-Users_clean.csv")
books <- read_csv("BX-Books_clean.csv")
ratings <- read_csv("BX-Book-Ratings_clean.csv")

uniqueTitle <- unique(books$bookTitle)
uniqueAuthor <- unique(books$bookAuthor)




book_recommendation <- function(input,input2,input3) {
  row_num <- which(books[,2] == input)
  row_num2 <- which(books[,2] == input2)
  row_num3 <- which(books[,2] == input3)
  userSelect <- matrix(NA,8552)
  userSelect[row_num] <- 5 #hard code first selection to rating 5
  userSelect[row_num2] <- 4 #hard code second selection to rating 4
  userSelect[row_num3] <- 3 #hard code third selection to rating 3
  userSelect <- t(userSelect)
  
  ratingmat <- dcast(ratings, userID~ISBN, value.var = "bookRating", na.rm=FALSE)
  ratingmat <- ratingmat[,-1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect,ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)
  
  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")
  
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, ratingmat2[1], n=10)
  recom_list <- as(recom, "list")
  recom_result <- data.frame(matrix(NA,10))
  for (i in c(1:10)){
    recom_result[i,1] <- movies2[as.integer(recom_list[[1]][i]),2]
  }
  colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
  return(recom_result)
}

book_recommendation(uniqueTitle[1:1000],uniqueTitle[1:1000],uniqueTitle[1:1000])
#uniquePublisher <- unique(books$publisher)
#uniqueYear <- unique(books$yearOfPublication)

#create visualization - author breakdown
#=============
#book ratings
#============
bookRating <- ratings %>% 
  inner_join(books,by = "ISBN") %>%
  select (bookTitle, bookAuthor, bookRating)
#aggregate book ratings
bookRating2 <- bookRating %>%
  group_by(bookTitle, bookAuthor) %>%
  summarise(bookRatingTotal = sum(bookRating), n=n())

bookRating2$AverageRate <- bookRating2$bookRatingTotal/bookRating2$n

bookRatingTop <- bookRating2 %>% 
  arrange(desc(AverageRate)) %>%
  head(n = 10L) %>%
  select(bookTitle, bookAuthor, AverageRate)
  
#top 10 book ratings visualization
p<-ggplot(data=bookRatingTop, aes(x=bookTitle, y=AverageRate)) +
  geom_bar(stat="identity")
# Horizontal bar plot
p + coord_flip()

#============
#publishing years
#============
bookPublishing <- books %>% 
  select (bookAuthor, yearOfPublication) %>%
  group_by(bookAuthor,yearOfPublication) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(n = 10L)
p<-ggplot(data=bookPublishing, aes(x=yearOfPublication, y=n)) +
  geom_bar(stat="identity") 
p
  
#============
#age breakdown
#============
bookAge <- ratings %>%
  inner_join(books, by="ISBN") %>%
  inner_join(users, by="userID") %>%
  select(bookAuthor, Age)
# Basic histogram
ggplot(bookAge, aes(x=Age)) + geom_histogram()
# Change the width of bins
ggplot(bookAge, aes(x=Age)) + 
  geom_histogram(binwidth=10)

#============
#location breakdown
#============
bookLocation <- ratings %>%
  inner_join(books, by="ISBN") %>%
  inner_join(users, by="userID") %>%
  select(bookAuthor, Location)

bookLocation2 <- separate(bookLocation, Location, 
                          into = c("area", "location","country"), sep = ",")

bookLocation3 <- bookLocation2 %>%
  group_by(bookAuthor, country) %>%
  tally()



