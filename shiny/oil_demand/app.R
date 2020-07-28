# Objective
# create a visualization that shows
# the current state of world mobility as it pertains 
# to energy consumption, but also allows a user to drill down into details. 
# https://www.apple.com/covid19/mobility

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
        -c('alternative_name',
            'country',
            'sub-region',
            'country',
            '2020-05-11',
            '2020-05-12'
        )
    )


mobility_df <- df %>%
  pivot_longer(-c(geo_type, region, transportation_type),
               names_to = "date_info",
               values_to = "sum")

region_info <- sort(unique(df$region))
transportation_type_info <- sort(unique(df$transportation_type))
geo_info <- sort(unique(df$geo_type))

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
                    sidebarPanel(
                    selectInput("geoInput", "Geography type", choices = geo_info),
                    selectInput("regionInput", "Region", choices = NULL),
                    checkboxGroupInput("transportationInput",
                                       label = "Transportation types",
                                       choices = transportation_type_info,
                                       selected = transportation_type_info)),
                    mainPanel(
                        fluidRow(
                        #Change in routing requests since January 13, 2020
                        h2("Mobility Trends", style = "text-align: center;"),
                        plotOutput("countryOutput")
                    ))
                ))
    ))
)
server <- function(input, output, session) {
    
  region_df <- reactive({
    region_df <- mobility_df %>%
      filter(geo_type == input$geoInput) %>%
      select(region) %>%
      distinct()
  })  
  
  
  observe({
    updateSelectInput(session = session, inputId = "regionInput", choices = region_df())
  })
      
    
    output$countryOutput <- renderPlot({
       data_df <- mobility_df %>%
         filter(geo_type == input$geoInput) %>%
         filter(region == input$regionInput) %>%
         filter(transportation_type %in% input$transportationInput)
         
         
         ggplot(data_df, aes(x=date_info, y=sum, group=transportation_type)) +
         geom_line(aes(color=transportation_type))+
         geom_point(aes(color=transportation_type)) + theme_classic() + 
         theme(legend.text = element_text(size = 10),
               legend.title = element_text(size = 10),
               axis.title = element_text(size = 15),
               axis.text = element_text(size = 10),
               axis.text.x = element_text(angle = 45, hjust = 1)) +
         labs(x = "Date", y = "Distance") +
         labs (fill = "Transportation type")
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)