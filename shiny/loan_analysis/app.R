packages <-
    c('tidyverse', 'shiny', 'shinydashboard', 'scales', 'DT', 'dplyr')

#load packages
for (package in packages) {
    if (!require(package, character.only = T, quietly = T)) {
        install.packages(package)
        library(package, character.only = T)
    }
}

loan_data <- c(250, 500, 750, 1000, 1250)
payment_data <- c("weekly", 'bi-weekly', 'monthly')

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
                        selectInput("paymentTypeInput", "Payment Period", choices = payment_data),
                        dateInput("dateInput", "Loan Orignination Date", value = "2020-01-01"),
                        sliderInput(
                            "loanTermInput",
                            "Loan Term (in weeks)",
                            min = 0,
                            max = 360,
                            value = 36
                        ),
                        sliderInput(
                            "loanInput",
                            "Loan Amount ($)",
                            min = 250,
                            max = 1000,
                            value = 500
                        ),
                        sliderInput(
                            "subscriptionInput",
                            "Subscription ($)",
                            min = 0,
                            max = 50,
                            value = 10
                        ),
                        sliderInput(
                            "aprInput",
                            "APR (%)",
                            min = 0,
                            max = 100,
                            value = 20
                        )
                    ),
                    mainPanel(fluidRow(
                        h2("Loan Analysis", style = "text-align: center;"),
                        DT::dataTableOutput("summaryOutput"),
                        DT::dataTableOutput("mainOutput"),
                    ))
                ))
    ))
)

#generate function to perform calculation it will have a for loop + 5 inputs
#

payment_schedule <- function(paymentType, dateBegin, loanTerm, subscriptionAmount, loaAmount, 
                             subscriptionAmount, apr){
    for (num in 0:loanTerm){
        
    }
    
}

server <- function(input, output, session) {
    # for (num in 1:input$loanTermInput) {
    #     if (input$paymentTypeInfo == "weekly"){
    #         
    #     } else if (input$paymentTypeInfo == "bi-weekly"){
    #         
    #     } else if (input$paymentTypeInfo == "monthly"){
    #         
    #     }
    # }
    
    output$summaryOutput <- DT::renderDataTable(DT::datatable({
        
    }))
    
    output$mainOutput <- DT::renderDataTable(DT::datatable({
        
    }))
}


# Run the application
shinyApp(ui = ui, server = server)
