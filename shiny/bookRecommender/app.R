###########################
# Book recommender system
###########################

###########################
# Packages 
###########################
packages <- c('shiny', 'recommenderlab','proxy','reshape2')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

###########################
# Load code and data
###########################
source("helpercode.R")

books <- read_csv("BX-Books_clean.csv")
ratings <- read_csv("BX-Book-Ratings_clean.csv")
bookIds <- length(unique(books$ISBN)) #271360
ratingbookIds <- length(unique(ratings$ISBN)) #149836
books2 <- books[-which((bookIds %in% ratingbookIds) == FALSE),]
books2 <- books2 %>%
  top_n(5000,ISBN)

###########################
# UI
###########################
ui <- fluidPage(
    tabsetPanel(
      tabPanel("Intro",
               includeMarkdown("intro.md"),
               hr()),
      tabPanel("Book Recommender", fluid = TRUE,
                 mainPanel(
                   fluidRow(
                     h3("Choose Three books You Like"),
                     column(5,
                            selectInput("select", label = "Book 1",
                                        choices = as.character(books2$bookTitle[1:1000])),
                            selectInput("select2", label = "Book 2",
                                        choices = as.character(books2$bookTitle[1001:2000])),
                            selectInput("select3", label = "Book 3",
                                        choices = as.character(books2$bookTitle[2001:3000])),
                            submitButton("Submit")
                      ),
                  column(7,
                            h3("You Might Like These Too!"),
                            DT::dataTableOutput("table")
                         )
                     )
                  )
      )
  )
)

server = function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    book_recommendation(input$select, input$select2, input$select3)
  })) 
}


# Run the application 
shinyApp(ui = ui, server = server)

