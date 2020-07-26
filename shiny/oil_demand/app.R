

#packages
packages <-
    c('ggplot2',
      'corrplot',
      'tidyverse',
      'shiny',
      'shinydashboard',
      'scales')
#load packages
for (package in packages) {
    if (!require(package, character.only = T, quietly = T)) {
        install.packages(package)
        library(package, character.only = T)
    }
}

#load data
df <- read_csv("applemobilitytrends-2020-07-23.csv")

df <-
    select (
        df,
        -c(
            'geo_type',
            'alternative_name',
            'country',
            'sub-region',
            'country',
            '2020-05-11',
            '2020-05-12'
        )
    )


mobility_df <- df %>%
    pivot_longer(-c(region, transportation_type),
                 names_to = "date_info",
                 values_to = "sum")

region_info <- sort(unique(df$region))
transportation_type_info <- sort(unique(df$transportation_type))
day_info <- sort(unique(mobility_df$date_info))


ui <- dashboardPage(
    dashboardHeader(title = "Mobility Trends"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Introduction",
            tabName = "Introduction",
            icon = icon("dashboard")
        ),
        menuItem("Trends", tabName = "trends", icon = icon("th"))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "Introduction", includeMarkdown("readme.md"), hr()),
        tabItem(tabName = "trends",
                sidebarLayout(
                    sidebarPanel(selectInput("regionInfo", "Country", choices = region_info),),
                    mainPanel(fluidRow(
                        #Change in routing requests since January 13, 2020
                        h2("Mobility Trends", style = "text-align: center;"),
                        plotOutput("countryOutput")
                    ))
                ))
    ))
)
server <- function(input, output) {
    
    output$countryOutput <- renderPlot({
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)