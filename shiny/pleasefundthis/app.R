#kickstarter analysis
rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix',
              'data.table','vtreat', 'rsample')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read.csv("PleaseFundThis.csv")

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Kickstarter.com Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("dashboard")),
            menuItem("Summary", tabName = "summary", icon = icon("th")),
            menuItem("Country Analysis", tabName = "country", icon = icon("th")),
            menuItem("City Analysis", tabName = "city", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "summary",
                    mainPanel(
                        h2("Summary",style="text-align: center;"),
                        fluidRow(
                            valueBoxOutput("monthlyPriceOutput"),
                        ),
                        h3("Explanation",style="text-align: center;"),
                        fluidRow(
                            infoBoxOutput("rateOutput")
                        ),
                        fluidRow(
                            infoBoxOutput("projectCostOuput") 
                        ),
                        fluidRow(
                            infoBoxOutput("userCostOutput")
                        )
                    )),
            tabItem(tabName = "country",),
            tabItem(tabName = "city",)
        )
    )
)

# Define server logic 
server <- function(input, output,session) {
    output$monthlyPriceOutput <- renderValueBox({
        infoBox(
            "You should charge your users: ", paste0(""), icon = icon("list"),
            color = "blue"
        )
    })
}

shinyApp(ui, server)
            



