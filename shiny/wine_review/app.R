
#wine selector varietal + price + rating + top 10 outputs table code
#wine recommendation using user layout + recommendation
#================================================================================
# Shiny web app which provides insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines
# using data from wineenthusisat
#================================================================================

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','countrycode',
              'highcharter',"gridExtra")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
load("wine_dfR.RData")

#dropdown information
variety <- sort(as.vector(unique(wine_df$variety)))
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
                selectInput("priceInput", 
                            label = "Prices",
                            choices = price_ranges)
              ),
              mainPanel(
                h3("Ratings"),
                plotOutput("ratingPriceplot"),
                h3("Variety"),
                plotOutput("varietyPriceplot"),
                h3("Country"),
                plotOutput("countryPriceplot")
              )
     ), 
     tabPanel("Ratings",
            h1("Ratings",style="text-align: center;"),
            sidebarPanel(
              selectInput("ratingInput", 
                          label = "Ratings",
                          choices = rating )
            ),
            mainPanel(
              h3("Price"),
              plotOutput("priceRatingplot"),
              h3("Variety"),
              plotOutput("varietyRatingplot"),
              h3("Country"),
              plotOutput("countryRatingplot")
            )
    ),
    tabPanel("Variety",
          h1("Variety",style="text-align: center;"),
          sidebarPanel(
            selectInput("varietyInput", 
                        label = "Variety",
                        choices =variety )
          ),
          mainPanel(
            h3("Price"),
            plotOutput("priceVarietyplot"),
            h3("Rating"),
            plotOutput("ratingVarietyplot"),
            h3("Country"),
            plotOutput("countryVarietyplot")
          )
    ),
    tabPanel("Country",
          h1("Country",style="text-align: center;"),
          sidebarPanel(
            selectInput("countryInput", 
                        label = "Country",
                        choices =country )
          ),
          mainPanel(
            h3("Price"),
            plotOutput("priceCountryplot"),
            h3("Variety"),
            plotOutput("varietyCountryplot"),
            h3("Ratings"),
            plotOutput("ratingCountryplot")
          )
    ),
    tabPanel("Wine Selector",
      h1("Wine Selector",style="text-align: center;"),
      sidebarPanel(
        selectInput("variety", 
                    label = "Choose a variety:",
                    choices = variety,
                    selected = "All"),
        radioButtons("pricerange", 
                     label = "Select a price range:",
                     choices = price_ranges),
        checkboxGroupInput("pointcategory",
                           label = "Select desired rating(s):",
                           choices = rating,
                           selected = rating
      ), 
      mainPanel(
        dataTableOutput("selected_wines"), width = 10)
      )
    ),
    tabPanel("Wine Recommendation",
      h1("Wine Recommendation",style="text-align: center;")
    )
   )

)


# Define server logic 
server <- function(input, output) {
  
  #Price 
  output$ratingPriceplot <- renderPlot({
    
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(price_range == input$priceInput)
    
    ggplot(wine_info, aes(x = factor(ratings, levels=rating))) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Rating", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
  })
  
  #plotOutput("varietyPriceplot"),
  output$varietyPriceplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(price_range == input$priceInput) %>%
      summarise(total_count=n())
    
    ggplot(wine_info, aes(x = variety,y=total_count)) + 
      geom_tile(color = "black", size = 0.5) +
      theme(panel.border = element_rect(size = 2),
            plot.title = element_text(size = rel(1.2)),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right")
  })
  
  
  output$countryPriceplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country) %>%
      filter(price_range == input$priceInput)
    
    wine_info <- wine_info %>%
    mutate(country = str_replace_all(country, c("England" = "UK")))
    
    highchart(type = "map") %>%
      hc_add_series_map(worldgeojson,
                        wine_info %>% 
                          group_by(country,price_range) %>% 
                          summarise(total = n()) %>% 
                          ungroup() %>%
                          mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                        value = "total", joinBy = "iso2") %>%
      hc_title(text = "Prices by Country") %>%
      hc_colorAxis(minColor = "steelblue", maxColor = "blue")
    
  })
  
  #Rating

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
  
  
  #plotOutput("varietyRatingplot"),
  output$varietyRatingplot <- renderPlot({})
  
  
  output$countryRatingplot <- renderPlot({
    wine_info <- wine_df %>%
      select(ratings,country) %>%
      filter(ratings == input$ratingInput)
    
    wine_info <- wine_info %>%
      mutate(country = str_replace_all(country, c("England" = "UK")))
    
    highchart(type = "map") %>%
      hc_add_series_map(worldgeojson,
                        wine_info %>% 
                          group_by(country,ratings) %>% 
                          summarise(total = n()) %>% 
                          ungroup() %>%
                          mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                        value = "total", joinBy = "iso2") %>%
      hc_title(text = "Ratings by Country") %>%
      hc_colorAxis(minColor = "steelblue", maxColor = "blue")
  })
  
  
  #Variety
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
  
  
  #plotOutput("ratingVarietyplot"),
  output$ratingVarietyplot <- renderPlot({})
  
  
  
  output$countryVarietyplot <- renderPlot({
    wine_info <- wine_df %>%
      select(country, variety) %>%
      filter(variety == input$varietyInput)
    
    wine_info <- wine_info %>%
      mutate(country = str_replace_all(country, c("England" = "UK")))
    
    highchart(type = "map") %>%
      hc_add_series_map(worldgeojson,
                        wine_info %>% 
                          group_by(country,variety) %>% 
                          summarise(total = n()) %>% 
                          ungroup() %>%
                          mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                        value = "total", joinBy = "iso2") %>%
      hc_title(text = "Variety by Country") %>%
      hc_colorAxis(minColor = "steelblue", maxColor = "blue")
  })  
  
  
  
  #Country
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
   
  output$varietyCountryplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(country == input$countryInput) %>%
      top_n(200) %>%
      arrange(desc(variety))
    
    ggplot(wine_info, aes(x = variety)) + 
      geom_bar(width = 0.5, fill="steelblue") + theme_classic() + 
      labs(x = "Variety", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15))
    
  })
    
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

