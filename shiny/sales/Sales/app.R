#clear data
rm(list = ls())
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
superstore <- read_excel(file_path)
newdata <- superstore[order(superstore$State),]

state <- unique(newdata$State)

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Sales Information by State"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "State:", 
                  choices=state)
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("salesCategory")#,
      #plotOutput("heightWeight"),
      #plotOutput('gameMedalCount'), 
      #plotOutput("sportMedalCount")
    )
    
  )
)

server <- function(input, output) { 
  
  output$salesCategory <- renderPlot({
    graphdata <- newdata %>%
      filter(State %in% input$nameInfo)
    
    ggplot(data=graphdata, aes(x=Category, y=Sales)) + 
      ggtitle("Sales by Category") + theme_bw()

  })
  
  }

shinyApp(ui, server)