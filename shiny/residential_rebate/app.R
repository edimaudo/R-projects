rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse',
              'shiny','shinydashboard','shinyWidgets',
              'dplyr','readxl')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}


df <- read_excel("FY_Summary.xlsx") #load data
df[is.na(df)] <- 0 #replace na with 0
area <- c(sort(unique(df$Area)))
fiscal_year <- c(sort(unique(df$`Fiscal Year`)))
rebate_type <- c(sort(unique(df$Type)))
colnms=c("July","August","September","October","November",
         "December","January","February","March","April","May","June")
df$month_total <- rowSums(df[,colnms]) #aggregate months

#app
ui <- dashboardPage(
    dashboardHeader(title = "Water Service Program"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction", icon = icon("th")),
            menuItem("Summary", tabName = "summary", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "introduction",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "summary",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("fiscalYearInput", "Fiscal Year", choices = fiscal_year),
                            selectInput("areaInput", "Area", choices = area)
                        ),
                        mainPanel(
                            h2("Rebate Summary",style="text-align: center;"), 
                            DT::dataTableOutput("rebateOutput")
                        )
                    )
                    )
        )
    )
    
)

#server info
server <- function(input, output) {

    output$rebateOutput <- DT::renderDataTable({
        df_output <- df %>%
            group_by(Area,Type,`Fiscal Year`,`Type Detail`) %>%
            filter(Area == input$areaInput,`Fiscal Year` == input$fiscalYearInput) %>%
            summarise(total = sum(month_total))
        DT::datatable(df_output)
    })

}

shinyApp(ui, server)