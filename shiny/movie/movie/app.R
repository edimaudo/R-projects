

library(shinydashboard)
library(shiny)

#read data
file_path <- "movie.csv"
movie <- read.csv(file_path, stringsAsFactors = FALSE)
movie <- movie[complete.cases(movie), ]

#get genres
genres <- strsplit(movie$Genre, ",")
genres <- unlist(genres)
genres <- unique(genres)

#get directors
directors<- unique(movie$Director)

#get actors
actors <- strsplit(movie$Actors, ",")
actors <- unlist(actors)
actors <- unique(actors)

#genre choices
genre_choices <- append(genres, "Select All", after = 0)
directors_choices <- append(directors, "Select All", after = 0)
actors_choices <- append(actors, "Select All", after = 0)

#dashboard title
header <- dashboardHeader(title = "The Movie App") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    sliderInput("Rating", "Movie according to the rating",0,10,c(0,10)),
    sliderInput("Year", "Year released", 2006, 2016, c(2006, 2016)),
    selectInput("genre_input", "Genre (a movie can have multiple genres)",genre_choices, selected = "Select All"),
    selectInput("directors_input","Director",directors_choices, selected = "Select All"),
    selectInput("actors_input","Actor",actors_choices, selected = "Select All")
  )
)



ui <- dashboardPage(
  header,
  sidebar,
  dashboardBody()
)






# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

