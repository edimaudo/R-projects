################
# Denso Sales Analysis
################

################
# Load packages
################
rm(list = ls()) #clear environment
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

################
# Load data
################
df <- read_excel("denso.xlsx")


################
# Application UI
################

#------------------
# UI dropdowns
#------------------
customer_info <- c(sort(unique(df$`Customer Name`)))
year_info <- c(sort(unique(df$Year)))


ui <- dashboardPage(
    dashboardHeader(title = "Company Insights"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Customer", tabName = "customer", icon = icon("th")), 
            menuItem(" Customer Comparison", tabName = "compare", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
        #------------------
        # Company
        #------------------
        tabItem(tabName = "customer",
                sidebarLayout(
                    sidebarPanel(
                        selectInput("customerInput", "Customer", choices = customer_info),
                        sliderInput("yearInput","Year",min=min(year_info),max=max(year_info),
                                value = c(min(year_info),max(year_info)),step =1,ticks = FALSE)
                    ),
                    mainPanel(
                        h2("Customer",style="text-align: center; font-style: bold;"), 
                        fluidRow(
                            valueBoxOutput("salesBox"),
                            valueBoxOutput("quantityBox")
                        ), 
                        
                    )
                )
            ),
        tabItem(tabName = "compare",
                sidebarLayout(
                    sidebarPanel(
                        selectInput("customerInput", "Customer", choices = customer_info),
                        sliderInput("yearInput","Year",min=min(year_info),max=max(year_info),
                                    value = c(min(year_info),max(year_info)),step =1,ticks = FALSE)
                    ),
                    mainPanel(
                        h2("Comparison",style="text-align: center; font-style: bold;"), 
                        
                        )
                        
                    )
                )
        )
        )
    )         
 

################
# Server
################
server <- function(input, output,session) {
    
    all_data_sum <- function() {
        temp_df <- df %>%
          summarise(sales_total = sum(`Sales Amount (Actual)`)) %>%
        select(sales_total)
        return (temp_df)
    }
    
    #------------------
    # sales box
    #------------------
    output$salesBox <- renderValueBox({
         
            sales_sum_df <- df %>%
                filter(`Customer Name` == input$customerInput,
                       Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
                summarise(sales_total = sum(`Sales Amount (Actual)`)) %>%
                select(sales_total)
        
        
        valueBox(
            paste0(sales_sum_df), "Total Sales ($)", icon = icon("credit-card"),
            color = "blue"
        )
    })
    
    output$quantityBox <- renderValueBox({
        
        quantity_sum_df <- df %>%
            filter(`Customer Name` == input$customerInput,
                   Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
            summarise(quantity_total = sum(Qty)) %>%
            select(quantity_total)
        
        
        valueBox(
            paste0(quantity_sum_df), "Total Quantity", icon = icon("list"),
            color = "blue"
        )
    })
    
    
    
}


shinyApp(ui, server)

