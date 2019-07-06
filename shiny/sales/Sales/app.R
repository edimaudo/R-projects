## app.R ##
#packages
packages <- c("tidyverse",'shiny','shinydashboard','readxl')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
file_path <- "Sample - Superstore.xls"
superstore <- readxl(file.choose())



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
  )
)

server <- function(input, output) { }

shinyApp(ui, server)