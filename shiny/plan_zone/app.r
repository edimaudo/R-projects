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
        helpText("Select a Price"),
        selectInput("priceInput", label = "Prices",choices = price_ranges)
    ),
    mainPanel(
        fluidRow(
            h3("Ratings",style="text-align: center;"),
            plotOutput("ratingPriceplot"),
        )
    )
)


# Define server logic 
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)