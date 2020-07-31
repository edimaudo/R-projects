packages <-
    c('tidyverse','shiny','shinydashboard','scales','DT','dplyr')

#load packages
for (package in packages) {
    if (!require(package, character.only = T, quietly = T)) {
        install.packages(package)
        library(package, character.only = T)
    }
}


ui <- dashboardPage(
    dashboardHeader(title = "Loan Analysis"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Introduction",
            tabName = "Introduction",
            icon = icon("dashboard")
        ),
        menuItem("Loan analysis", tabName = "analysis", icon = icon("th"))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "Introduction", includeMarkdown("readme.md"), hr()),
        tabItem(tabName = "analysis",
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
                            h2("Loan Analysis", style = "text-align: center;"),
                            DT::dataTableOutput("selectedWinesOutput")
                        ))
                ))
    ))
)



server <- function(input, output, session) {}


# Run the application 
shinyApp(ui = ui, server = server)
