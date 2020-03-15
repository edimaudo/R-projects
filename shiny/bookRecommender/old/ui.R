#
# This is a data explorer and Book recommender system
# 

#design intro page
#design the tabs
#design the filters
#design the visualizations
#update the different pages with the visualizations
#design the code for the recommender system
#test the recommender system


#remove old data
rm(list=ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','shiny')
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

bookAuthor <- unique(books$bookAuthor)
sort(bookAuthor)

# Set up the application ui
shinyUI(navbarPage("Book Explorer and Recommendation System",
                   tabPanel("Intro",
                              includeMarkdown("intro.md"),
                            hr()),
                   tabPanel("Author",
                            fluidRow(column(12,
                                            h1("Author Information"),
                                            br(),
                                            h4("Instructions"),
                                            p("Get author information"))),
                            hr(),
                            fluidRow(sidebarPanel(width = 3,
                            h4("Author?"),
                            helpText("Use the dropdown for author information"),
                            selectInput("authorInfo", "Author:", choices=bookAuthor)),
                            mainPanel(plotOutput("AuthorPlot", height = 500)))),
                            
                   tabPanel("Book Recommendation",
                            
                            hr())
))
