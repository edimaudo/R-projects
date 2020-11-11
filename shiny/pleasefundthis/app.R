#kickstarter analysis
rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix',
              'data.table','vtreat', 'rsample','scales')
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

cities <- sort(as.vector(unique(df$city)))

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Kickstarter Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("dashboard")),
            menuItem("Summary", tabName = "summary", icon = icon("list-alt")),
            menuItem("City Analysis", tabName = "city", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "summary",
                    mainPanel(
                        h2("Summary",style="text-align: center;"),
                        fluidRow(valueBoxOutput("countryOutput"),
                                 valueBoxOutput("ciytOutput")),
                        fluidRow(
                            valueBoxOutput("categoryOutput"),
                            valueBoxOutput("subCategoryOutput")
                        ),
                        fluidRow(valueBoxOutput("amountRaisedOutput"),
                                 valueBoxOutput("percentofSuccessfulProjectsOutput")
                        )
                    )
            ),
            tabItem(tabName = "city",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("cityInput", "City", choices = cities)
                        ),
                        mainPanel(
                            h2("City Analysis",style="text-align: center;"), 
                        )
                    )
        )
    )
    )
)


# Define server logic 
server <- function(input, output,session) {
    
    output$countryOutput <- renderValueBox({
        valueBox(
            paste0(length(unique(df$region))),"# of Countries", 
            icon = icon("list"),
            width = 12,
            color = "blue"
        )
    })

    output$ciytOutput <- renderValueBox({
        valueBox(
            paste0(length(unique(df$city))),"# of cities",
            icon = icon("list"),
            width = 12,
            color = "blue"
        )
    })

    output$categoryOutput <- renderValueBox({
        valueBox(
            paste0(length(unique(df$major_category))),"# of categories", icon = icon("list"),
            width = 12,
            color = "aqua"
        )
    })
    
    output$subCategoryOutput <- renderValueBox({
        valueBox(
            paste0(length(unique(df$minor_category))),"# of sub-categories", icon = icon("list"),
            width = 12,
            color = "aqua"
        )
    })
    

    output$amountRaisedOutput <- renderValueBox({
        value1 = round(sum(df$amt_pledged_.),0)
        valueBox(
            paste0(value1),"Amount raised in $", 
            icon = icon("credit-card"),
            width = 12,
            color = "blue"
        )
    })
    
    output$percentofSuccessfulProjectsOutput <- renderValueBox({
        value1 <- no_success / length(df$project_success)
        valueBox(
            paste0(round(value1*100 , 2),"%"), "% of successful projects ",
            icon = icon("thumbs-up"),
            width = 12,
            color = "blue"
        )
    })
    
    

    
    
}

shinyApp(ui, server)
            



