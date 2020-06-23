
#================================================================================
# Shiny web app which provides insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines
# using data from wineenthusisat
#================================================================================

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','scales','ggfortify','DT')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
load("wine_dfR.RData")

variety <- sort(as.vector(unique(wine_df$variety)))
variety <- na.omit(variety)
rating <- c("Good","Very Good","Superb","Excellent")
country <- sort(as.vector(unique(wine_df$country)))
price_ranges <- c('< $10','$10-$25','$25-$50', '$50-$100','$100-$500', '> $500')
#refactor price ranges
wine_df$price_range <- factor(wine_df$price_range,levels = price_ranges)
#add rating column
wine_df <- wine_df %>%
  mutate(ratings = case_when(wine_df$point_range == '< 85' ~ 'Good',
                             wine_df$point_range == '85-90' ~ 'Very Good',
                             wine_df$point_range == '90-95' ~ 'Excellent',
                             wine_df$point_range == '95-100' ~ 'Superb'))

rating_info <- c(sort(unique(wine_df$ratings)))


# Define UI for application
ui <- fluidPage(
   tabsetPanel(
     tabPanel("Introduction",
       includeMarkdown("intro.md")
     ),
     tabPanel("Background",
              h1("Background",style="text-align: center;"),
              mainPanel(
                br(),
                h4("* Over 120,000 reviews, 20 different wine tasters"),
                h4("* More than 110,000 different wines from 42 countries"),
                h4("* 691 wines were rated"),
                h4("* Wines priced at $4-$3300 per bottle")
              )
     ),
     tabPanel("Prices",
              h1("Prices",style="text-align: center;"), 
              sidebarPanel(
                helpText("Select a Price"),
                selectInput("priceInput", label = "Prices",choices = price_ranges)
              ),
              mainPanel(
                fluidRow(
                  h3("Ratings",style="text-align: center;"),
                  plotOutput("ratingPriceplot"),
                ),
                fluidRow(
                  h3("Top 10 Varieties",style="text-align: center;"),
                  plotOutput("varietyPriceplot"),
                )
              )
     ), 
     tabPanel("Ratings",
            h1("Ratings",style="text-align: center;"),
            sidebarPanel(
              selectInput("ratingInput", label = "Ratings",choices = rating )
            ),
            mainPanel(
              fluidRow(
                h3("Price",style="text-align: center;"),
                plotOutput("priceRatingplot")           
              ),
              fluidRow(
                h3("Top 10 Varieties",style="text-align: center;"),
                plotOutput("varietyRatingplot")              
              )
  
            )
    ),
    tabPanel("Variety",
          h1("Variety",style="text-align: center;"),
          sidebarPanel(
            selectInput("varietyInput", label = "Variety",choices =variety )
          ),
          mainPanel(
            h3("Price",style="text-align: center;"),
            plotOutput("priceVarietyplot"),
            h3("Rating",style="text-align: center;"),
            plotOutput("ratingVarietyplot"),
          )
    ),
    tabPanel("Country",
          h1("Country",style="text-align: center;"),
          sidebarPanel(
            selectInput("countryInput", label = "Country",choices =country )
          ),
          mainPanel(
            h3("Price",style="text-align: center;"),
            plotOutput("priceCountryplot"),
            h3("Top 10 Varieties",style="text-align: center;"),
            plotOutput("varietyCountryplot"),
            h3("Ratings",style="text-align: center;"),
            plotOutput("ratingCountryplot")
          )
    ),
    tabPanel("Wine Selector",
      h1("Wine selector",style="text-align: center;"),
      sidebarPanel(
        selectInput("countryAllInput", 
                    label = "Choose a Country:",
                    choices = country),
        radioButtons("pricerangeInput", 
                     label = "Select a price range:",
                     choices = price_ranges),
        checkboxGroupInput("ratingAllInput",
                           label = "Select desired rating(s):",
                           choices = rating_info,
                           selected = rating_info)
      ), 
      mainPanel(DT::dataTableOutput("selectedWinesOutput"))
    )
   )
)


# Define server logic 
server <- function(input, output) {
  
  #Rating & Price
  output$ratingPriceplot <- renderPlot({
    
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(price_range == input$priceInput)
    
    ggplot(wine_info, aes(x = factor(ratings, levels=rating))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Rating", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 15))
  })
  
  #variety & price
  output$varietyPriceplot <- renderPlot({
    
    wine_info <- wine_df %>%
      group_by(variety) %>%
      filter(price_range == input$priceInput) %>%
      summarise(total_count=n()) %>%
      select(variety,total_count) %>%
      arrange(desc(total_count)) %>%
      top_n(10)
    
    ggplot(wine_info, aes(x = variety ,y = total_count)) + 
      geom_bar(stat = "identity",width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Variety", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 15),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #Rating & Price
  output$priceRatingplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(ratings == input$ratingInput)
    
    ggplot(wine_info, aes(x = factor(price_range, levels=price_ranges))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Prices", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  })
  
  #Variety & Rating
  output$varietyRatingplot <- renderPlot({
    wine_info <- wine_df %>%
      group_by(variety) %>%
      filter(ratings == input$ratingInput) %>%
      summarise(total_count=n()) %>%
      select(variety,total_count) %>%
      arrange(desc(total_count)) %>%
      top_n(10)
    
    ggplot(wine_info, aes(x = variety ,y = total_count)) + 
      geom_bar(stat = "identity",width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Variety", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 15),
            axis.text.x = element_text(angle = 45, hjust = 1))    
  })
  
  
  #Variety & Price
  output$priceVarietyplot <- renderPlot({
    
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(variety == input$varietyInput)
    
    ggplot(wine_info, aes(x = factor(price_range, levels=price_ranges))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Prices", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  })
  
  
  # Variety & Rating
  output$ratingVarietyplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(variety == input$varietyInput)
    
    ggplot(wine_info, aes(x = factor(price_range, levels=price_ranges))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Prices", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  })
  
  
  #Country & Price
  output$priceCountryplot <- renderPlot({
    
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(country == input$countryInput)
    
    ggplot(wine_info, aes(x = factor(price_range, levels=price_ranges))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Prices", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  }) 
  
  #Country & Variety
  output$varietyCountryplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(country == input$countryInput) %>%
      top_n(10) %>%
      arrange(desc(variety))
    
    ggplot(wine_info, aes(x = variety)) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Variety", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
    
  })
    
  #Country & Rating
  output$ratingCountryplot <- renderPlot({
    
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(country == input$countryInput)
    
    ggplot(wine_info, aes(x = factor(ratings, levels=rating))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Rating", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  })
  

  
  #Wine selector
  output$selectedWinesOutput <- DT::renderDataTable(DT::datatable({
    wine_info <- wine_df %>%
      filter( price_range == input$pricerangeInput, 
             ratings %in% input$ratingAllInput, 
             country == input$countryAllInput) %>%
      group_by(title, variety) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      mutate("Wines" = title) %>%
      #top_n(20) %>%
      select(Wines,variety)
      
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

