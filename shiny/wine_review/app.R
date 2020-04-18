#country plot for rating, prices and variety sections
#wine selector varietal + price + rating + top 10 outputs table code

#code & #test
#have variety dropdown + visualization for rating,  prices
#have price drop down + visualization for rating, & variety
#have rating drop down + visualization for prcies,  & variety



#use boxes and summary information layout + code + test

#wine recommendation using user layout + recommendation code + test
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
   includeCSS("www/main.css"),
   
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
  
  output$varietyPriceplot <- renderPlot({
    wine_info <- wine_df %>%
      select(price_range,country,ratings, variety) %>%
      filter(price_range == input$priceInput)
    
    ggplot(wine_info, aes(variety, price_range)) + geom_tile(aes(fill = variety), 
                                                             colour = "white") 
  })
  
  
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
  
  
  output$varietyRatingplot <- renderPlot({})
  
  
  output$priceVarietyplot <- renderPlot({})
  
  
  output$ratingVarietyplot <- renderPlot({})
  
  
  output$priceCountryplot <- renderPlot({})
  
  
  output$varietyCountryplot <- renderPlot({})
  
  
  output$ratingCountryplot <- renderPlot({})

}

# Run the application 
shinyApp(ui = ui, server = server)

