#
# This is a data explorer and Book recommender system
# 


#remove old data
rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','shiny', 'recommenderlab','proxy','reshape2')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
users <- read_csv("BX-Users_clean.csv")
books <- read_csv("BX-Books_clean.csv")
ratings <- read_csv("BX-Book-Ratings_clean.csv")

bookTitles <- unique(books$bookTitle)

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
  recommender_model <- Recommender(ratingmat2, method = "UBCF",
                                   param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, ratingmat2[1], n=10)
  recom_list <- as(recom, "list")
  recom_result <- data.frame(matrix(NA,10))
  for (i in c(1:10)){
    recom_result[i,1] <- movies2[as.integer(recom_list[[1]][i]),2]
  }
  colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
  return(recom_result)
}


ui <- fluidPage(
    navbarPage("Book Recommendation System",
    tabsetPanel(
      tabPanel("Intro",
               includeMarkdown("intro.md"),
               hr()),
      tabPanel("Book Recommender", fluid = TRUE,
                 mainPanel(
                   fluidRow(
                     column(5,
                            selectInput("select", 
                                        label = h3("Choose Three books You Like"),
                                        choices = as.character(bookTitles[1:1000])),
                            
                            selectInput("select2", label = NA,
                                        choices = as.character(bookTitles[1:1000])),
                            
                            selectInput("select3", label = NA,
                                        choices = as.character(bookTitles[1:1000])),
                            
                            submitButton("Submit")),
                     column(7,
                            h3("You Might Like These Too!"),
                            tableOutput("table"))
                     ))))))
  
server = function(input, output) {
  output$table <- renderTable({
    book_recommendation(input$select, input$select2, input$select3)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

