

library(shinydashboard)
library(shiny)

#read data
file_path <- "movie.csv"
movie <- read.csv(file_path, stringsAsFactors = FALSE)
movie <- movie[complete.cases(movie), ]



# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

