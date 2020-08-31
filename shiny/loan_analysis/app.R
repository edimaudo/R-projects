rm(list=ls())

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
                        h3("Summary",style="text-align: center;"),
                        DT::dataTableOutput("summaryOutput"),
                        h3("Details",style="text-align: center;"),
                        DT::dataTableOutput("mainOutput"),
                    ))
                ))
    ))
)

#generate function to perform calculation it will have a for loop + 5 inputs
#

payment_schedule <- function(paymentType, dateBegin, loanTerm, subscriptionAmount, 
                             loanAmount, apr){
    
    payment_data_colinfo <- c('Date','Daily_Opening_Balance','Payment',
                              'Daily_Closing_Balance','Daily_Interest','Subscription')
    payment_data <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(payment_data) <- payment_data_colinfo
    
    #starting values
    dayCount <- 0
    dateInfo <- dateBegin
    dailyOpeningBalance <- 0
    payment <- 0
    dailyClosingBalance <- loanAmount + subscriptionAmount
    dailyInterest <- dailyClosingBalance * ((apr*0.01)/365)
    subscription <- subscriptionAmount
    
    for (num in 0:loanTerm){
        if (num == 0){
            payment_data_day <- data.frame(dateInfo,dailyOpeningBalance,payment,dailyClosingBalance,
                                           dailyInterest, subscription)
            colnames(payment_data_day ) <- payment_data_colinfo
            payment_data <- rbind(payment_data,payment_data_day)
            
        } else {
            if (paymentType == 'weekly'){
                
            } else if (paymentType == "bi-weekly"){
                
            } else if (paymentType == "monthly"){
                
            }
            payment_data_day <- data.frame(dateInfo,dailyOpeningBalance,payment,dailyClosingBalance,
                                           dailyInterest, subscription)
            colnames(payment_data_day ) <- payment_data_colinfo
            payment_data <- rbind(payment_data,payment_data_day)
        }

    }
    
}

server <- function(input, output, session) {
    
    output$summaryOutput <- DT::renderDataTable(DT::datatable({
        #payment_schedule()
    }))
    
    output$mainOutput <- DT::renderDataTable(DT::datatable({
        
    }))
}


# Run the application
shinyApp(ui = ui, server = server)
