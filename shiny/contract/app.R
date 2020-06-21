
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv("Recent_Contract_Awards.csv")

agency <- c(sort(unique(df$AgencyName)))
category <- c(sort(unique(df$AgencyName)))
selectMethod <- c(sort(unique(df$SelectionMethodDescription)))


# Define UI for application
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
      tabItem(tabName = "agency",),
      tabItem(tabName = "category",),
      tabItem(tabName = "selectionMethod",)
    )
  )
)



server <- function(input, output) {

  
  }

# Run the application 
shinyApp(ui = ui, server = server)