packages <-
    c('tidyverse','shiny','shinydashboard','scales','DT','dplyr')

#load packages
for (package in packages) {
    if (!require(package, character.only = T, quietly = T)) {
        install.packages(package)
        library(package, character.only = T)
    }
}




server <- function(input, output, session) {}


# Run the application 
shinyApp(ui = ui, server = server)
