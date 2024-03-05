#================================================================================
# Shiny web app which provides insights into wine quality
# using data from UCI Machine learning repository
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
              'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','dplyr',
              'mlbench','caTools','gridExtra','doParallel','grid',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate',
              'data.table', 'rsample','scales'
              )
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
red_df <- read.csv("winequality-red.csv",sep=";")
white_df <- read.csv("winequality-white.csv",sep=";")
red_df$quality <- ifelse(red_df$quality < 6, 'bad', 'good')
red_df$quality <- as.factor(red_df$quality)
white_df$quality <- ifelse(white_df$quality < 6, 'bad', 'good')
white_df$quality <- as.factor(white_df$quality)
################
#Define UI for application
################
ui <- dashboardPage(
  dashboardHeader(title = "Wine Quality Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Wine Quality Prediction", tabName = "prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("about.md"),hr()),
      tabItem(tabName = "overview",
              sidebarLayout(
                sidebarPanel(
                  selectInput("wineTypeInput", "Wine Type", choices = c("Red","White"), selected = "Red"),
                  submitButton("Submit")
                ),
                mainPanel(
                  h3("Wine Quality distribution",style="text-align: center;"),
                  plotOutput("WineQualityPlot"),
                  h3("Wine Properties Correlation",style="text-align: center;"),
                  plotOutput("WineCorrelationPlot"),  
                  h3("Quality vs pH",style="text-align: center;"),
                  plotOutput("QualitypHPlot"),
                  h3("Quality vs Alcohol Content",style="text-align: center;"),
                  plotOutput("QualityAlcoholPlot")
                )
              )
      ),
      tabItem(tabName="prediction",
              mainPanel(
                h2("Wine Quality Prediction",style="text-align:center;")
              )
              )
    )
  )
)


      
################
# Define server logic 
################
server <- function(input, output,session) {
  
  
df <- reactive({
  if (input$wineTypeInput == 'White'){
    white_df
  } else {
    red_df
  }
})  
  

 
  
  output$WineQualityPlot <- renderPlot({
    
    wine_info <- df() %>%
    group_by(quality) %>%
    summarise(total_count=n()) %>%
    select(quality,total_count)
    
    ggplot(wine_info, aes(x = quality ,y = total_count)) + 
      geom_bar(stat = "identity",width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Variety", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 15))  
    
  })
  
  output$WineCorrelationPlot <- renderPlot({
    
    corr_df <- df() %>%
      select (- quality)
    
    corrplot(cor(corr_df),method="number")
    
  })
  
  
  output$QualitypHPlot <- renderPlot({
    
    wine_info <- df()
    ggplot(wine_info, aes(x = quality, y = pH, fill = quality)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.07)
    
  })
  
  
  output$QualityAlcoholPlot <- renderPlot({
    
    wine_info <- df()
    ggplot(wine_info, aes(x = quality, y = y, alcohol = quality)) +
      geom_beeswarm(cex = 3)
    
  })
  
}

shinyApp(ui, server)