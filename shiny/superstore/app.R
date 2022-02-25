# Super Store
rm(list = ls()) #clear data

###############
# Packages
###############
packages <- c("tidyverse",'shiny','shinydashboard','readxl','ggplot2','dplyr')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
###############
# Load data
###############
file_path <- "Sample - Superstore.xls"
superstore <- read_excel(file_path,sheet="Orders")

###############
# UI
###############

# UI Dropdown
state <- c(sort(unique(superstore $State)))

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  # Give the page a title
  titlePanel("Sales Information by State"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "State:", choices=state)
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("salesCategory"),
      plotOutput("salesProfit"),
      plotOutput('salesSubCategory')
    )
  )
)

###############
# Server
###############
server <- function(input, output) { 
  
  output$salesCategory <- renderPlot({
    graphdata <- superstore %>%
      dplyr::filter(State %in% input$nameInfo)
    ggplot(data=graphdata, aes(x=Category, y=Sales)) + geom_bar(stat = "identity", width = 0.3) +
      ggtitle("Sales by Category") + theme_bw()
    
    
  
  })
  
  output$salesProfit <- renderPlot({
    graphdata <- superstore %>%
      dplyr::filter(State %in% input$nameInfo)
    ggplot(data=graphdata, aes(x=Category, y=Profit)) + geom_bar(stat = "identity", width = 0.3) +
      ggtitle("Profit by Category") + theme_bw()    
  })

  output$salesSubCategory <- renderPlot({
    graphdata <- superstore %>%
      dplyr::filter(State %in% input$nameInfo)
    ggplot(data=graphdata, aes(x=`Sub-Category`, y=Sales)) + geom_bar(stat = "identity", width = 0.3) +
      ggtitle("Sales by Category") + theme_bw() + 
      theme(
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))
  })  

  }

shinyApp(ui, server)