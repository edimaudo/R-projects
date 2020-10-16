#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'DT','data.table','readxl','dplyr')
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
    dashboardHeader(title = "Sales Invoice Generator"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Sales Invoice", tabName = "salesInvoice", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "salesInvoice",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("saleID", "SaleID:", choices = invoice),
                            br(),
                            submitButton("Submit")
                        ),
                        mainPanel(fluidRow(
                            h2("Sales Invoice", style = "text-align: center;"),
                            DT::dataTableOutput("salesOutput")
                        ))
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    output$salesOutput <- DT::renderDataTable(DT::datatable({
        data <- df %>%
            filter(SaleID == input$saleID)
            
    }))
}

shinyApp(ui, server)
