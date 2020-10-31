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

no_success <- df %>%
    select(project_success)%>%
    filter(project_success == TRUE)%>%
    summarise(count_success = n())



# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Kickstarter Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("dashboard")),
            menuItem("Summary", tabName = "summary", icon = icon("th")),
            menuItem("City Analysis", tabName = "city", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "summary",
                    mainPanel(
                        h2("Summary",style="text-align: center;"),
                        fluidRow(infoBoxOutput("countryOutput"),
                                 infoBoxOutput("ciytOutput")),
                        fluidRow(
                            infoBoxOutput("categoryOutput"),
                            infoBoxOutput("subCategoryOutput")
                        ),
                        fluidRow(infoBoxOutput("amountRaisedOutput"),
                                 infoBoxOutput("percentofSuccessfulProjectsOutput")
                        )
                    )
            ),
            tabItem(tabName = "country",),
            tabItem(tabName = "city",)
        )
    )
)

# Define server logic 
server <- function(input, output,session) {
    
    output$countryOutput <- renderValueBox({
        infoBox(
            "# of Countries: ", paste0(length(unique(df$region))), icon = icon("list"),
            color = "blue"
        )
    })

    output$cityOutput <- renderValueBox({
        infoBox(
            "# of cities: ", paste0(length(unique(df$city))), icon = icon("list"),
            color = "blue"
        )
    })

    output$categoryOutput <- renderValueBox({
        infoBox(
            "# of categories: ", paste0(length(unique(df$major_category))), icon = icon("list"),
            color = "blue"
        )
    })
    
    output$subCategoryOutput <- renderValueBox({
        infoBox(
            "# of sub-categories: ", paste0(length(unique(df$minor_category))), icon = icon("list"),
            color = "blue"
        )
    })
    

    output$amountRaisedOutput <- renderValueBox({
        infoBox(
            "Amount raised: $", paste0(sum(df$amt_pledged_.)), icon = icon("list"),
            color = "blue"
        )
    })
    
    output$percentofSuccessfulProjectsOutput <- renderValueBox({
        infoBox(
            "% of successful projects ", paste0(no_success / length(df$project_success)), icon = icon("list"),
            color = "blue"
        )
    })
    


    
    
}

shinyApp(ui, server)
            



