################
# Denso Sales Analysis
################

################
# Packages
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
# UI drop-downs
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
                            valueBoxOutput("partsBox"),
                            valueBoxOutput("quantityBox")
                        ),
                        fluidRow(
                            tabBox(
                                title = "Year Insights",
                                id = "tabset2", 
                                width = "100%",
                                selected = "Sales",
                                tabPanel("Sales", plotOutput("salesPlot", height = 150)),
                                tabPanel("Revenue", plotOutput("revenuePlot", height = 150))
                            ) 
                        ), 
                        fluidRow(
                            DT::dataTableOutput("partsTable") 
                        )
                    )
                ) 
            ),
        tabItem(tabName = 'compare',
                sidebarLayout(
                    sidebarPanel(
                        selectInput("customerCompareInput1", "Customer 1", choices = customer_info),
                        selectInput("customerCompareInput2", "Customer 2", choices = customer_info,
                                    selected = "Abdullah Center"),
                        sliderInput("yearCompareInput","Year",min=min(year_info),max=max(year_info),
                                    value = c(min(year_info),max(year_info)),step =1,ticks = FALSE)
                    ),
                    mainPanel(
                        h2("Comparison",style="text-align: center; font-style: bold;"),
                        fluidRow(
                            tabBox(
                                title="Customers",
                                id = "tabset3",
                                width = "100%",
                                selected = "Sales",
                                tabPanel("Sales", plotOutput("salesComparePlot", height = 150)),
                                tabPanel("Revenue", plotOutput("revenueComparePlot", height = 150)),
                                tabPanel("Parts", plotOutput("partsComparePlot", height = 150)),
                                tabPanel("Quantity", plotOutput("quantityComparePlot", height = 150))
                            )
                        )
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
    
    
    #------------------
    # Quantity box
    #------------------
    output$quantityBox <- renderValueBox({
        
        quantity_sum_df <- df %>%
            filter(`Customer Name` == input$customerInput,
                   Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
            summarise(quantity_total = sum(Qty)) %>%
            select(quantity_total)
        
        
        valueBox(
            paste0(quantity_sum_df), "Parts Quantity", icon = icon("list"),
            color = "blue"
        )
    })
    
    #------------------
    # Parts box
    #------------------
    output$partsBox <- renderValueBox({
        
        parts_df <- df %>%
            filter(`Customer Name` == input$customerInput,
                   Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
            summarise(parts_total = n_distinct(`Item No.`)) %>%
            select(parts_total)
        
        
        valueBox(
            paste0(parts_df), "Parts", icon = icon("list"),
            color = "blue"
        )
    })

    #------------------
    # Sales visualization
    #------------------
    output$salesPlot <- renderPlot({
     
    sales_df <- df %>% 
        filter(`Customer Name` == input$customerInput,
               Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
        group_by(Year) %>%
        summarise(Sales = sum(`Sales Amount (Actual)`)) %>%
        select(Sales,Year)
        
        ggplot(sales_df, aes(as.factor(Year),Sales)) + 
            geom_bar(stat="identity", width = 0.5, fill="#bc5090") +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Sales") + 
            theme(legend.text = element_text(size = 10),
                  legend.title = element_text(size = 10),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 10),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    #------------------
    # Revenue visualization
    #------------------
    output$revenuePlot <- renderPlot({
        revenue_df <- df %>% 
            mutate(Revenue = `Sales Amount (Actual)` * Qty) %>%
            filter(`Customer Name` == input$customerInput,
                   Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
            group_by(Year) %>%
            summarise(Revenue = sum(Revenue)) %>%
            select(Revenue,Year)
        
        ggplot(revenue_df, aes(as.factor(Year),Revenue)) + 
            geom_bar(stat="identity", width = 0.5, fill="#ff6361") +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Sales") + 
            theme(legend.text = element_text(size = 10),
                  legend.title = element_text(size = 10),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 10),
                  axis.text.x = element_text(angle = 00, hjust = 1))
    })   
    
    #------------------
    # Parts information
    #------------------
    output$partsTable <- DT::renderDataTable({
        parts_df <- df %>%
            filter(`Customer Name` == input$customerInput,
                   Year >= input$yearInput[1] & Year <= input$yearInput[2]) %>%
            mutate(Revenue = `Sales Amount (Actual)` * Qty) %>%
            group_by(`Item No.`, Year) %>%
            summarise(Revenue = sum(Revenue), Sales = sum(`Sales Amount (Actual)`), 
                      Quantity = sum(Qty)) %>%
            arrange(desc(Revenue)) %>%
            select(`Item No.`,Year,Sales, Quantity, Revenue) 
        
        DT::datatable(parts_df)
        
    })
    
    
    
    
    #------------------
    # Sales compare plot
    #------------------
    output$salesComparePlot <- renderPlot({
        
        sales_df <- df %>% 
            filter(`Customer Name` %in% c(input$customerCompareInput1,input$customerCompareInput2),
                   Year >= input$yearCompareInput[1] & Year <= input$yearCompareInput[2]) %>%
            group_by(Year,`Customer Name`) %>%
            summarise(Sales = sum(`Sales Amount (Actual)`)) %>%
            select(Sales,Year,`Customer Name`)
        
        ggplot(sales_df, aes(as.factor(Year),Sales),color = `Customer Name`) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, 
                     aes(fill = `Customer Name`)) +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Sales", color="Customer") + 
            theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(angle = 00, hjust = 1))

    })
    #------------------
    # Revenue compare plot
    #------------------   
    output$revenueComparePlot <- renderPlot({
        
        sales_df <- df %>% 
            mutate(Revenue = `Sales Amount (Actual)` * Qty) %>%
            filter(`Customer Name` %in% c(input$customerCompareInput1,input$customerCompareInput2),
                   Year >= input$yearCompareInput[1] & Year <= input$yearCompareInput[2]) %>%
            group_by(Year,`Customer Name`) %>%
            summarise(Revenue = sum(Revenue)) %>%
            select(Revenue,Year,`Customer Name`)
        
        ggplot(sales_df, aes(as.factor(Year),Revenue),color = `Customer Name`) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, 
                     aes(fill = `Customer Name`)) +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Revenue", color="Customer") + 
            theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })   
    #------------------
    # Parts compare plot
    #------------------  
    output$partsComparePlot <- renderPlot({
        
        sales_df <- df %>% 
            filter(`Customer Name` %in% c(input$customerCompareInput1,input$customerCompareInput2),
                   Year >= input$yearCompareInput[1] & Year <= input$yearCompareInput[2]) %>%
            group_by(Year,`Customer Name`) %>%
            summarise(Parts= n_distinct(`Item No.`)) %>%
            select(Parts,Year,`Customer Name`)
        
        ggplot(sales_df, aes(as.factor(Year),as.integer(Parts)),color = `Customer Name`) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, 
                     aes(fill = `Customer Name`)) +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Parts", color="Customer") + 
            theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    }) 
    
    #------------------
    # Quantity compare plot
    #------------------  
    output$quantityComparePlot <- renderPlot({
        
        sales_df <- df %>% 
            filter(`Customer Name` %in% c(input$customerCompareInput1,input$customerCompareInput2),
                   Year >= input$yearCompareInput[1] & Year <= input$yearCompareInput[2]) %>%
            group_by(Year,`Customer Name`) %>%
            summarise(Quantity = sum(Qty)) %>%
            select(Quantity,Year,`Customer Name`)
        
        ggplot(sales_df, aes(as.factor(Year),Quantity),color = `Customer Name`) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, 
                     aes(fill = `Customer Name`)) +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Total Quantity", color="Customer") + 
            theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    }) 
    
}


shinyApp(ui, server)

