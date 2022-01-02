################
# packages  
################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','scales',
              'dplyr','mlbench','caTools', 'forecast','lubridate')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
################
# load data
################
df <- read_csv("Recent_Contract_Awards.csv")

df <- df %>%
  dplyr::select(AgencyName,CategoryDescription,SelectionMethodDescription,ContractAmount)

df <- na.omit(df)



################
# Define UI for application
################

#-----------------
# UI dropdowns
#-----------------
agency <- c(sort(unique(df$AgencyName)))
category <- c(sort(unique(df$CategoryDescription)))
selectMethod <- c(sort(unique(df$SelectionMethodDescription)))

ui <- dashboardPage(
  dashboardHeader(title = "Contract Information"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
      menuItem("Agency", tabName = "agency", icon = icon("th")),
      menuItem("Category", tabName = "category", icon = icon("th")),
      menuItem("selection Method", tabName = "selectionMethod", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Introduction",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "agency",
              sidebarLayout(
                sidebarPanel(
                  selectInput("agencyInfo", "Agency",choices=agency),
                ), 
                mainPanel(
                  fluidRow(
                    h2("Categories",style="text-align: center;"),
                    plotOutput("agencyCategoryOutput")
                  ),
                  fluidRow(
                    h2("Selection Methods",style="text-align: center;"),
                    plotOutput("agencySelectionMethodOutput")
                  )
                )
              )
              ),
      tabItem(tabName = "category",
              sidebarLayout(
                sidebarPanel(
                  selectInput("categoryInfo","Category",choices = category)
                ),
                mainPanel(
                  fluidRow(
                    h2("Agencies",style="text-align: center;"),
                    plotOutput("CategoryAgencyOutput")
                  ),
                  fluidRow(
                    h2("Selection Methods",style="text-align: center;"),
                    plotOutput("CategorySelectionMethodOutput")
                  )
                )
              )
              ),
      tabItem(tabName = "selectionMethod",
              sidebarLayout(
                sidebarPanel(
                  selectInput("selectionInput","Selection Method",choices = selectMethod)
                ),
                mainPanel(
                  fluidRow(
                    h2("Agencies",style="text-align: center;"),
                    plotOutput("SelectionMethodAgencyOutput")
                  ),
                  fluidRow(
                    h2("Categories",style="text-align: center;"),
                    plotOutput("SelectionMethodCategoryOutput")
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
server <- function(input, output) {
  
  output$agencyCategoryOutput <- renderPlot({
    
    agencyCategory_df <- df %>%
      group_by(CategoryDescription) %>%
      filter(AgencyName == input$agencyInfo) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount)) %>%
      select(CategoryDescription,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(x=CategoryDescription, y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Category Description", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$agencySelectionMethodOutput <- renderPlot({
    
    agencySelectionMethod_df <- df %>%
      group_by(SelectionMethodDescription) %>%
      filter(AgencyName == input$agencyInfo) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount),desc(SelectionMethodDescription)) %>%
      select(SelectionMethodDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=as.factor(SelectionMethodDescription), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Selection Method", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  
  output$CategoryAgencyOutput <- renderPlot({
    
    agencyCategory_df <- df %>%
      group_by(AgencyName) %>%
      filter(CategoryDescription == input$categoryInfo) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount)) %>%
      select(AgencyName,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(x=AgencyName, y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Agency", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$CategorySelectionMethodOutput <- renderPlot({
    
    agencySelectionMethod_df <- df %>%
      group_by(SelectionMethodDescription) %>%
      filter(CategoryDescription == input$categoryInfo) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount),desc(SelectionMethodDescription)) %>%
      select(SelectionMethodDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=as.factor(SelectionMethodDescription), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Selection Method", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  output$SelectionMethodAgencyOutput <- renderPlot({
    
    agencyCategory_df <- df %>%
      group_by(AgencyName) %>%
      filter(SelectionMethodDescription == input$selectionInput) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount)) %>%
      select(AgencyName,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(x=AgencyName, y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Agency", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$SelectionMethodCategoryOutput <- renderPlot({
    
    agencySelectionMethod_df <- df %>%
      group_by(CategoryDescription) %>%
      filter(SelectionMethodDescription == input$selectionInput) %>%
      summarize(totalAmount = sum(ContractAmount)) %>%
      arrange(desc(totalAmount),desc(SelectionMethodDescription)) %>%
      select(CategoryDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=as.factor(CategoryDescription), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() +
      labs(x = "Selection Method", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  }
################
# Run the application 
################
shinyApp(ui = ui, server = server)