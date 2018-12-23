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
      plotOutput("ageSex"),
      plotOutput("heightWeight"),
      plotOutput("medalCount"),
      plotOutput('gameMedalCount'), 
      plotOutput("sportMedalCount")
    )
    
  )
)



# Define a server for the Shiny app
server <- function(output,input){

    output$ageSex <- renderPlot({
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      ggplot(data=graphdata, aes(x=Age, na.rm=TRUE)) + geom_bar(aes(fill = Sex)) + xlab("Age") + 
        ggtitle("Age Vs Sex") + theme_bw()

    })
    
    output$heightWeight <- renderPlot({
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      ggplot(data=graphdata, aes(x=Height, y=Weight)) +
        geom_point() + xlab("Height") + ylab("Weight") + 
        ggtitle("Weight Vs Height") + theme_bw()
      
    })
    
    output$medalCount <- renderPlot({
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      graphdata <- graphdata %>%
        filter(Medal != "NA")
      ggplot(data=graphdata, aes(x=Medal, na.rm=TRUE)) + geom_bar() + xlab("Medals") +
        ggtitle("Medal Count by podium finish for all current olympics") + theme_bw()
    })
    
    output$gameMedalCount <- renderPlot({
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      graphdata <- graphdata %>%
        filter(Medal != "NA")
      ggplot(graphdata, aes(x=factor(Games) , na.rm=TRUE)) + geom_bar(aes(fill = Medal)) + 
        xlab("Olympic games") + 
        ggtitle("Games Medal count") + theme_bw()
    })
    
    output$sportMedalCount <- renderPlot({
      graphdata <- newdata %>%
        filter(Country %in% input$nameInfo)
      graphdata <- graphdata %>%
        filter(Medal != "NA")
      ggplot(data=graphdata, aes(x=Sport , na.rm=TRUE)) + geom_bar(aes(fill = Medal)) + 
        ylab("Sport events") + 
        ggtitle("Sport Medal count") + theme_bw()
    })
    
    
  
}

shinyApp(ui,server)
