#kickstarter analysis
rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate',
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

cities <- sort(as.vector(unique(df$city)))
categories <- sort(as.vector(unique(df$major_category)))

df$date_launched <- as.Date(df$date_launched, format = "%d/%m/%Y")
df$year <- as.numeric(format(df$date_launched, '%Y'))

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Kickstarter Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("th")),
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
                        fluidRow(valueBoxOutput("countryOutput"),
                                 valueBoxOutput("ciytOutput"),
                                  valueBoxOutput("categoryOutput")),
                        fluidRow(
                            valueBoxOutput("amountRaisedOutput"),
                            valueBoxOutput("percentofSuccessfulProjectsOutput"),
                            valueBoxOutput("subCategoryOutput")
                        )
                    )
            ),
            tabItem(tabName = "city",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("cityInput", "City", choices = cities),
                            selectInput("categoryInput", "Category", choices = categories),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h2("City Analysis",style="text-align: center;"), 
                            fluidRow(
                            h3("Amount pledged",style="text-align: center;"),
                            plotOutput("pledgeYearOutput"),
                            h3("# of pledges",style="text-align: center;"),
                            plotOutput("pledgenumYearOutput")
                            #plotOutput(""),
                            )
                        )
                    )
        ) # prediction model
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
    
    # number of pledges by year
    output$pledgenumYearOutput <- renderPlot({
        
        data_df <- df %>%
            filter(city == input$cityInput) %>%
            filter(major_category == input$categoryInput) %>%
            group_by(year) %>%
            summarise(total_pledges = sum(number_of_pledgers))
        
        ggplot(data = data_df,aes(x = as.factor(year),y = total_pledges)) +
            geom_bar(stat = "identity", width = 0.3) + theme_light() +
            labs(x = "Years",
                 y = "Total # of Pledges") +
            scale_y_continuous(labels = comma) +
            scale_x_discrete() +
            theme(
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 10),
                axis.title = element_text(size = 15),
                axis.text = element_text(size = 10),
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
        
        
        
    })
    
    #amount pledge by year
    output$pledgeYearOutput <- renderPlot({
        
        data_df <- df %>%
            filter(city == input$cityInput) %>%
            filter(major_category == input$categoryInput) %>%
            group_by(year) %>%
            summarise(total_pledges = sum(amt_pledged_.))
        
        ggplot(data = data_df,aes(x = as.factor(year),y = total_pledges)) +
            geom_bar(stat = "identity", width = 0.3) + theme_light() +
            labs(x = "Years",
                 y = "Amount Pledged ($)") +
            scale_y_continuous(labels = comma) +
            scale_x_discrete() +
            theme(
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 10),
                axis.title = element_text(size = 15),
                axis.text = element_text(size = 10),
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
        
        
        
    })
    

    

    

    
    
}

shinyApp(ui, server)
            



