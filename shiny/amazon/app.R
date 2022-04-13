################
# Packages
################
packages <- c('ggplot2','corrplot','tidyverse','readxl', 
              'RColorBrewer','shiny','shinydashboard','scales','dplyr',
              'forecast','lubridate','stopwords','tidytext','stringr',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}
################
# Load data
################
df <- read.csv("Amazon_Reviews_Vitamin_C.csv")
months <- c("January","February","March","April",'May',"June","July","August","September","October",
            "November","December")
#=============
# Text analytics
#=============


################
# UI
################

#----------------
#UI drop-downs
#----------------
product_info <- sort(unique(df$Procuct))
country_info <- sort(unique(df$Country))
month_info <- sort(unique(df$Month))
year_info <- sort(unique(df$Year))
rating_info <- sort(unique(df$Rating))

#----------------
#UI design
#----------------
ui <- dashboardPage(
    dashboardHeader(title = "Amazon Review Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("clock")),
            menuItem("Explore", tabName = "explore", icon = icon("th")),
            menuItem("Text Analysis", tabName = "text", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #===============
            # Summary
            #===============
            tabItem(tabName = "summary",
                        mainPanel(
                            h1("Summary",style="text-align: center;"), 
                            fluidRow(
                                valueBoxOutput("productBox", width = 3),
                                valueBoxOutput("countryBox",width = 3),
                                valueBoxOutput("yearBox",width = 3),
                                valueBoxOutput("ratingBox",width = 3),
                            ),
                            
                             tabsetPanel(type = "tabs",
                                         tabPanel(h4("Avg. Rating by Month",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgMonthRatingPlot")),
                                         tabPanel(h4("Average Rating by Year",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgYearRatingPlot"))
                             )
                        )
                    ),
                    
            
            #==============
            #Explore UI
            #==============
            tabItem(tabName = "explore",
                        mainPanel(
                            h1("Data Exploration",style="text-align: center;"),
                            DT::dataTableOutput("exploreTable")
                        )
            ), 
            #==============
            # Text analytics UI
            #==============
            tabItem(tabName = "text",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("ProductInput", "Product", choices = product_info, 
                                        selected="B000GG87V2"),
                            selectInput("CountryInput", "Country", choices = country_info,
                                        selected="United States "),
                            selectInput("monthInput", "Month", choices = month_info, 
                                        selected="January"),
                            selectInput("ratingInput", "Rating", choices = rating_info,
                                        selected="5"),
                            sliderInput("yearInput","Year",min=min(year_info),max=max(year_info),
                                        value = c(min(year_info),
                                                  max(year_info)),step =1,ticks = FALSE),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Text Analysis",style="text-align: center;"),
                            plotOutput("sentimentPlot"),
                            plotOutput("termFrequencyPlot"),
                            DT::dataTableOutput("topicTable")
                            
                            
                        )
                    )
            )

        )
    )
)

################
# Server
################

# Define server logic 
server <- function(input, output,session) {
    #===============
    # Summary logic
    #===============
    #--------------
    # Tab-boxes
    #--------------
    output$productBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Procuct))), "Products", icon = icon("list"),
            color = "green"
        )
    })
    
    output$countryBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Country))), "Countries", icon = icon("list"),
            color = "green"
        )
    })
    
    output$yearBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Year))), "Years", icon = icon("list"),
            color = "green"
        )
    })
    
    output$ratingBox <- renderValueBox({
        output <- mean(na.omit(df$Rating))
        output <- format(round(output, 1), nsmall = 2)
        valueBox(
            paste0(output), "Avg. Ratings", icon = icon("list"),
            color = "green"
        )
    })
    
    #--------------
    # Ratings plot
    #--------------
    output$avgMonthRatingPlot <- renderPlot({
        output_df <- df %>%
            mutate(Month = factor(Month, levels = months)) %>%
            group_by(Month) %>%
            summarise(avg_rating = mean(na.omit(Rating))) %>%
            select(Month, avg_rating)
        output_df <- na.omit(output_df)
        
        ggplot(output_df, aes(as.factor(Month),avg_rating)) + 
            geom_bar(stat="identity", width = 0.5, fill="#bc5090") + 
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Month", y = "Average Rating") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1)) 
        
    })
    
    output$avgYearRatingPlot <- renderPlot({
        output_df <- df %>%
            group_by(Year) %>%
            summarise(avg_rating = mean(na.omit(Rating))) %>%
            select(Year, avg_rating)
        output_df <- na.omit(output_df)
        
        ggplot(output_df, aes(as.factor(Year),avg_rating)) + 
            geom_bar(stat="identity", width = 0.5, fill="#bc5090") + 
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Average Rating") + 
            theme(legend.text = element_text(size = 13),
                  legend.title = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  axis.text = element_text(size = 13),
                  axis.text.x = element_text(angle = 00, hjust = 1)) 
    })
    
    #==============
    #Explore logic
    #==============
    output$exploreTable <- renderDataTable({
        output_df <- df %>%
            dplyr::select(Procuct,Country, Month, Year, Title, Body, Rating)
        DT::datatable(df,options = list(scrollX = TRUE))
    })
    
    
    #==============
    # Text analytics logic
    #==============
    output$sentimentPlot <- renderPlot({
        
    })
     
    termFrequencyPlot <- renderPlot({
        
    })
     
    output$topicTable <- renderDataTable({
        
    })
    
}


shinyApp(ui, server)