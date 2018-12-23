#load libraries
#packages
packages <- c("tidyverse",'shiny','shinydashboard')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
file_path <- "Olympics.csv"
olympics <- read.csv(file_path)

newdata <- olympics[order(olympics$Country),] 

country <- unique(newdata$Country)



# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Olympics by country"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "Country:", 
                  choices=country)
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("ageSex")  
    )
    
  )
)



# Define a server for the Shiny app
server <- function(output,input){


    output$ageSex <- renderPlot({
      
      #get the data
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      
      ggplot(data=graphdata, aes(x=Age, na.rm=TRUE)) + geom_bar(aes(fill = Sex)) + xlab("Age") + 
        ggtitle("Age Vs Sex") + theme_bw()

    })
  
}

shinyApp(ui,server)
