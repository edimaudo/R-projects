#================================================================================
# Shiny web app which provides insights into wine quality
# using data from UCI Machine learning repository
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
              'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','DT',
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

# clean up quality logic
red_df$quality <- ifelse(red_df$quality < 6, 'bad', 'good')
red_df$quality <- as.factor(red_df$quality)
white_df$quality <- ifelse(white_df$quality < 6, 'bad', 'good')
white_df$quality <- as.factor(white_df$quality)

################
#Load models
################
white_wine_model <- load("white_wine_model.RData")
red_wine_model <- load("red_wine_model.RData")

################
#Define UI for application
################
ui <- dashboardPage(
  dashboardHeader(title = "Wine Quality Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Wine Quality Insights", tabName = "prediction", icon = icon("th"))
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
              sidebarLayout(
                sidebarPanel(
                  selectInput("wineTypeInput", "Wine Type", choices = c("Red","White"), selected = "Red"),
                  sliderInput("fixedAcidityInput", "Fixed Acidity:",min = 1, max = 20, value = 2,step = 0.5),
                  sliderInput("volatileAcidityInput", "Volatile Acidity:",min = 0, max = 2, value = 0.5,step = 0.01),
                  sliderInput("citricAcidInput", "Citric Acid:",min = 0, max = 2, value = 0.1,step = 0.01),
                  sliderInput("residualSugarInput", "Residual Sugar",min = 0, max = 70, value = 1,step = 1.5),
                  sliderInput("chloridesInput", "Chlorides:",min = 0.01, max = 0.8, value = 0.01,step=0.01),
                  sliderInput("freeSulfurDioxideInput", "Free Sulfur Dioxide:",min = 1, max = 300, value = 5,step=10),
                  sliderInput("totalSulfurDioxideInput", "Total Sulphur Dioxide:",min = 5, max = 500, value = 15,step = 10),
                  sliderInput("densityInput", "Density:",min = 0.9, max = 1.5, value = 1,step = 0.0001),
                  sliderInput("phInput", "pH:",min = 2.5, max = 4, value = 2.5,step=0.05),
                  sliderInput("sulphatesInput", "Sulphates:",min = 0.1, max = 2, value = 0.15,step = 0.01),
                  sliderInput("alcoholInput", "Alcohol:",min = 8, max = 15, value = 8,step = 0.1),
                  submitButton("Submit")
                ), 
              mainPanel(
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Correlation", plotOutput("insightCorrelationPlot")),
                  tabPanel("ML Prediction", DT::dataTableOutput("predictionTable"))
                )
              ) 
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

filtered_df <- reactive({
  df() %>%
    filter(
      fixed.acidity >= input$fixedAcidityInput[1],
      volatile.acidity >= input$volatileAcidityInput[1],
      citric.acid >= input$citricAcidInput[1],
      residual.sugar >= input$residualSugarInput[1],
      chlorides >= input$chloridesInput[1],
      free.sulfur.dioxide >= input$freeSulfurDioxideInput[1],
      total.sulfur.dioxide >= input$totalSulfurDioxideInput[1],
      density  >= input$densityInput[1],
      pH>= input$phInput[1],
      sulphates >= input$sulphatesInput[1],
      alcohol >= input$alcoholInput[1]
    ) 
}) 


##==========
# Overview
##=========

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
    ggplot(wine_info, aes(x = quality, y = alcohol, fill = quality)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.07)
  })

##==========
# Insights
##=========  
  output$insightCorrelationPlot <- renderPlot({
    corr_df <- filtered_df() %>%
      select (- quality)
    corrplot(cor(corr_df),method="number")
  })
  
  # pH vs Density
  #output$insightpHDesnsityPlot <- renderPlot({})
  # Residual Sugar-Alcohol
  #output$insightResidualSugarPlot <- renderPlot({})
  
  ## ML Output
  output$predictionTable <- DT::renderDataTable({
    
    if (input$wineTypeInput == 'White'){
      predicted.classes <- white_wine_model %>% predict(filtered_df())  
    } else {
      predicted.classes <- red_wine_model %>% predict(filtered_df()) 
    }
    
    
  })
  
}




shinyApp(ui, server)