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
      dplyr::filter(AgencyName == input$agencyInfo) %>%
      dplyr::group_by(CategoryDescription) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount)) %>%
      dplyr::select(CategoryDescription,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(reorder(CategoryDescription,totalAmount), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() + 
      labs(x = "Category Description", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  
  output$agencySelectionMethodOutput <- renderPlot({
    
    agencySelectionMethod_df <- df %>%
      dplyr::filter(AgencyName == input$agencyInfo) %>%
      dplyr::group_by(SelectionMethodDescription) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount),desc(SelectionMethodDescription)) %>%
      dplyr::select(SelectionMethodDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=reorder(as.factor(SelectionMethodDescription),totalAmount) , 
                                               y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() + 
      labs(x = "Selection Method", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })
  

  
  output$CategoryAgencyOutput <- renderPlot({
    
    agencyCategory_df <- df %>%
      dplyr::group_by(AgencyName) %>%
      dplyr::filter(CategoryDescription == input$categoryInfo) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount)) %>%
      dplyr::select(AgencyName,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(x=reorder(AgencyName,totalAmount), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() + 
      labs(x = "Agency", y = "Contract Amount ($)") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete() +
      theme(legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 20, hjust = 1))
  })
  
  
  output$CategorySelectionMethodOutput <- renderPlot({
    
    agencySelectionMethod_df <- df %>%
      dplyr::group_by(SelectionMethodDescription) %>%
      dplyr::filter(CategoryDescription == input$categoryInfo) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount),desc(SelectionMethodDescription)) %>%
      dplyr::select(SelectionMethodDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=reorder(as.factor(SelectionMethodDescription),totalAmount)
                                               , y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() +
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
      dplyr::group_by(AgencyName) %>%
      dplyr::filter(SelectionMethodDescription == input$selectionInput) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount)) %>%
      dplyr::select(AgencyName,totalAmount)
    
    ggplot(data=agencyCategory_df , aes(x=reorder(AgencyName,totalAmount), y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() +
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
      dplyr::filter(SelectionMethodDescription == input$selectionInput) %>%
      dplyr::group_by(CategoryDescription) %>%
      dplyr::summarize(totalAmount = sum(ContractAmount)) %>%
      dplyr::arrange(desc(totalAmount)) %>%
      dplyr::select(CategoryDescription,totalAmount)
    
    ggplot(data=agencySelectionMethod_df , aes(x=reorder(as.factor(CategoryDescription),totalAmount) , 
                                               y=totalAmount)) +
      geom_bar(stat="identity", width = 0.4) + theme_classic() + coord_flip() + 
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