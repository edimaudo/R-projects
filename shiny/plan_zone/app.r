#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','ggfortify','shiny','readxl','DT')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read_excel("zone_data.xlsx")

#pytpe
ptype <- c(sort(unique(df$p_type)))
region <- c(sort(unique(df$region)))

ui <- fluidPage(
    h1("Unit analysis",style="text-align: center;"), 
    sidebarPanel(
        helpText("Select a Date"),
        dateInput("date", "Date", value = "2020-01-01"),
        checkboxGroupInput("pytpeInput",
                           label = "Select Ptype",
                           choices = ptype,
                           selected = ptype),
        checkboxGroupInput("regionInput",
                           label = "Select Region",
                           choices = region,
                           selected = region)
    ),
    mainPanel(
        fluidRow(
            h3("Ratings",style="text-align: center;"),
            DT::dataTableOutput("")
        )
    )
)


# Define server logic 
server <- function(input, output) {
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