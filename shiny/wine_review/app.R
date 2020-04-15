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
#have rating drop down + visualization for prcies, country & variety layout + code + test
#have price drop down + visualization for rating, country & variety layout + code + test
#have variety dropdown + visualization for rating, country, prices layout + code + test


#wine selector varietal + price + rating + top 10 outputs table layout + code 



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

variety <- as.vector(unique(wine_df$variety))

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
                selectInput("ratingInput", 
                            label = "Ratings",
                            choices =c("",""))
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
     tabPanel("Ratings",
            h1("Ratings",style="text-align: center;")          
    ),
    tabPanel("Variety",
      h1("Variety",style="text-align: center;")
    ),
    tabPanel("Country",
      h1("Country",style="text-align: center;")
    ),
    tabPanel("Wine Selector",
      h1("Wine Selector",style="text-align: center;")
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

