##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("Profitability.csv")

#----------------
# data updates
#----------------
df$DateComplete <- as.Date(df$DateComplete, format =  "%d/%m/%Y")
df$DateLogged <- as.Date(df$DateLogged, format =  "%d/%m/%Y")

##################
# UI
##################

#----------------
# UI dropdown
#----------------
date_info <- c(sort(unique(df$DateLogged)))
customer_info <- c('All',sort(unique(df$Customer)))

ui <- dashboardPage(skin = "yellow",
        dashboardHeader(title = "Platform analysis"),
            dashboardSidebar(
                sidebarMenu(
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
                )
            ),
            dashboardBody(
                tabItems(
                #----------------
                # Dashboard
                #----------------
                    tabItem(tabName = "dashboard",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("customerInput", "Customer", choices = customer_info),
                                    sliderInput("dateInput","Dates",min=min(date_info),
                                                max=max(date_info),
                                                value = c(min(date_info),max(date_info)),
                                                step =1,ticks = FALSE)   
                                  ),
                                mainPanel(
                                    h2("Insights",style="text-align: center; font-style: bold;"), 
                                    tabBox(
                                        title = "Profitability",
                                        id = "tabset1", 
                                        width = "100%",
                                        selected = "Sales",
                                        tabPanel("Sales", plotOutput("salesPlot", height = 150)),
                                        tabPanel("Revenue", plotOutput("revenuePlot", height = 150))
                                    ),
                                    plotOutput("jobStatusPlot", height = 250),
                                    

                                    
                                )
                            )
                        )
                    )
            )
)

##################
# Server
##################
server <- function(input, output, session) {
    
    # Job status plot
    output$jobStatusPlot <- renderPlot({
        job_status_df <- df %>%
            filter(DateLogged >= input$dateInput[1] & DateLogged <= input$dateInput[2]) %>%
            group_by(Status) %>%
            summarise(Total = n()) %>%
            select(Status,Total)
        
        ggplot(job_status_df, aes(reorder(Status,Total),Total),color = Status) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Status)) + 
            coord_flip() +theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Status", y = "Total") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
    })
    
    
    
}


shinyApp(ui, server)