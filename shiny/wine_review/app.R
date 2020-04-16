# review data - doen
# review what other people did - done
# design what visualization is needed - done
# design readme - done
# design about section for the app - done
# Review libraries to use - done
# Design the layout of the app - done 
# build initial layout - done
#build introduction - done
#create reminaing tabs layout - done
#- add it with centered layout - done
#layout
#have variety dropdown + visualization for rating, country, prices - done
#have price drop down + visualization for rating, country & variety - done
#have rating drop down + visualization for prcies, country & variety - done
#have country drop down + visualization for prcies, country & variety - done
#wine selector varietal + price + rating + top 10 outputs table layout - done

#code & #test
#have variety dropdown + visualization for rating,  prices
#have price drop down + visualization for rating, & variety
#have rating drop down + visualization for prcies,  & variety
#have country drop down + visualization for prcies,  & variety

#country plot for rating, prices and variety sections
#wine selector varietal + price + rating + top 10 outputs table code


#use boxes and summary information layout + code + test

#wine recommendation using user layout + recommendation code + test


#================================================================================
# Shiny web app which provides insights into prices, ratings, 
# descriptions, and geographic distribution of the world's most esteemed wines
# using data from wineenthusisat
#================================================================================

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','countrycode','highcharter',"gridExtra")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
wine_data <- load("wine_dfR.RData")

#dropdown information
variety <- sort(as.vector(unique(wine_df$variety)))
rating <- c("Good","Very Good","Superb","Excellent")
country <- sort(as.vector(unique(wine_df$country)))
price <- c('< $10','$10-$25','$25-$50', '$50-$100','$100-$500', '> $500 ')

  

#add column for ratings "Superb","Excellent","Very Good","Good" using point range

# Define UI for application that draws a histogram
ui <- fluidPage(
   tabsetPanel(
     
     tabPanel("Introduction",
       includeMarkdown("intro.md")
      
     ),
     tabPanel("Background",
              h1("Background",style="text-align: center;")
     ),
     tabPanel("Prices",
              h1("Prices",style="text-align: center;"), 
              sidebarPanel(
                selectInput("priceInput", 
                            label = "Prices",
                            choices = price )
              ),
              mainPanel(
                h3("Ratings"),
                plotOutput("ratingPriceplot", height = 200, width = 5),
                h3("Variety"),
                plotOutput("varietyPriceplot", height = 200,width = 5),
                h3("Country"),
                plotOutput("countryPriceplot", height = 200,width = 5)
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
              plotOutput("priceRatingplot", height = 200, width = 5),
              h3("Variety"),
              plotOutput("varietyRatingplot", height = 200,width = 5),
              h3("Country"),
              plotOutput("countryRatingplot", height = 200,width = 5)
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
            plotOutput("priceVarietyplot", height = 200, width = 5),
            h3("Rating"),
            plotOutput("ratingVarietyplot", height = 200,width = 5),
            h3("Country"),
            plotOutput("countryVarietyplot", height = 200,width = 5)
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
            plotOutput("priceCountryplot", height = 200, width = 5),
            h3("Variety"),
            plotOutput("varietyCountryplot", height = 200,width = 5),
            h3("Ratings"),
            plotOutput("ratingCountryplot", height = 200,width = 5)
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
                     choices = price),
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

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

