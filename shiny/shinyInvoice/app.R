#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'DT','data.table','readxl')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read_excel("Sales Detail Report.xlsx")
invoice <- sort(as.vector(unique(as.integer(df$SaleID))))

ui <- dashboardPage(
    dashboardHeader(title = "Side Project SaaS Price Generator"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Sales Invoice", tabName = "salesInvoice", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "priceGenerator",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("yearInput", "Year:", choices = yearData),
                            br(),
                            submitButton("Submit")
                        ),
                        mainPanel(fluidRow(
                            h2("Text mining insights", style = "text-align: center;"),
                            plotOutput("generateProgramSentiment")
                        ))
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    
}

shinyApp(ui, server)
