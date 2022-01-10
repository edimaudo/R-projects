################
# Packages
################
rm(list = ls()) #clear environment
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','SnowballC','wordcloud', 
              'RColorBrewer','shiny','shinydashboard','scales','dplyr',
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
df <- read_excel("otf.xlsx")
#--------------
# data information
#--------------
organization <- c(sort(unique(df$Organization_name)))
city <- c(sort(unique(df$Recipient_org_city_update)))
year <- as.integer(c(sort(unique(df$Fiscal_year_update))))
grant_program <- c(sort(unique(df$Grant_program)))
age_group <- c(sort(unique(df$Age_group_update)))
budget_fund <- c(sort(unique(df$Budget_fund_update)))
program_area <- c(sort(unique(df$Program_area_update)))
geo_area <- c(sort(unique(df$Geographical_area_served)))
population <- c(sort(unique(df$Population_served)))

################
# Application UI
################
#------------------
# UI
#------------------
ui <- dashboardPage(
  dashboardHeader(title = "OTF Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("th")), 
      menuItem("City", tabName = "city", icon = icon("th")),
      menuItem("Organization", tabName = "organization", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
#--------------
# Summary
#--------------
      tabItem(tabName = "summary",
        mainPanel(
          img(src='OTF5.png', height = "100" ,align = "right"),
            h2("Overview",style="text-align: left; font-style: bold;"), 
          tags$div(
            tags$p(
            tags$a(href="https://otf.ca", "Ontario Trillium Fund (OTF)"),
            "First is an agency of the Government of Ontario, and one of Canada’s leading 
            granting foundations. OTF awarded $108 million to 629 projects last year to 
            build healthy and vibrant communities in Ontario."
            ),
          ),
          fluidRow(
            valueBoxOutput("ageBox"),
            valueBoxOutput("cityBox"),
            valueBoxOutput("areaBox"),
            valueBoxOutput("populationBox"),
            valueBoxOutput("grantBox"),
            valueBoxOutput("programBox")
          ), 
          fluidRow(
            plotOutput("grantPlot", height = 300)
          )
        )
      ),
#--------------
# City
#--------------
      tabItem(tabName = "city",
              sidebarLayout(
                sidebarPanel(
                  selectInput("cityInput", "City", choices = city),
                  sliderInput("yearInput","Year",min=min(year),max=max(year),
                              value = c(min(year),max(year)),step =1,ticks = FALSE)
                ),
                mainPanel(
                  h2("City Insights",style="text-align: center; font-style: bold;"), 
                  #splitLayout(#),
                  fluidRow(
                    tabBox(
                      title = "Group Insights",
                      id = "tabset2", 
                      width = "100%",
                      selected = "Age Group",
                      tabPanel("Age Group", plotOutput("ageGroupCityPlot", height = 150, 
                                                       width='75%')),
                      tabPanel("Area Served", plotOutput("areaServedCityPlot", height = 150, 
                                                         width='75%')),
                      tabPanel("Population",  plotOutput("populationCityPlot", height = 150,
                                                         width='75%')),
                      tabPanel("Grant Program", plotOutput("grantProgramCityPlot", height = 150, 
                                                           width='75%')),
                      tabPanel("Program Area", plotOutput("programAreaCityPlot", height = 150, 
                                                          width='75%')),
                      tabPanel("Budget", plotOutput("budgetCityPlot", height = 150, 
                                                    width='75%')),
                      tabPanel("Organization", plotOutput("organizationCityPlot", height = 150, 
                                                          width='75%')),
                    ) 
                  ),
                  fluidRow(
                    plotOutput("grantCityPlot", height = 150)
                  )
              )
            )
      ),
#--------------
# Organization
#--------------
      tabItem(tabName = "organization",
              sidebarLayout(
                sidebarPanel(
                  selectInput("organizationInput", "Organization", choices = organization),
                  sliderInput("yearInput","Year",min=min(year),max=max(year),
                              value = c(min(year),max(year)),step =1,ticks = FALSE)
                ),
                mainPanel(
                  h2("Organization Insights",style="text-align: center; font-style: bold;"), 
                  fluidRow(
                    tabBox(
                      title = "Organization Insights",
                      id = "tabset3", 
                      width = "100%",
                      selected = "Age Group",
                      tabPanel("Age Group", plotOutput("ageGroupOrgPlot", height = 150, 
                                                       width='75%')),
                      tabPanel("Area Served", plotOutput("areaServedOrgPlot", height = 150, 
                                                         width='75%')),
                      tabPanel("Population",  plotOutput("populationOrgPlot", height = 150, 
                                                         width='75%')),
                      tabPanel("Grant Program", plotOutput("grantProgramOrgPlot", height = 150, 
                                                           width='75%')),
                      tabPanel("Program Area", plotOutput("programAreaOrgPlot", height = 150, 
                                                          width='75%')),
                      tabPanel("Budget", plotOutput("budgetOrgPlot", height = 150, 
                                                    width='75%')),
                      tabPanel("City", plotOutput("cityOrgPlot", height = 150,
                                                  width='75%')),
                    ) 
                  ),
                  fluidRow(
                    plotOutput("grantOrgPlot", height = 150)
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
  
#===============
# Summary
#===============
#--------------
# Tab-boxes
#--------------
  output$ageBox <- renderValueBox({
    valueBox(
      paste0(length(age_group)), "Age Groups", icon = icon("list"),
      color = "purple"
    )
  })
  output$cityBox <- renderValueBox({
    valueBox(
      paste0(length(city)), "Cities", icon = icon("list"),
      color = "purple"
    )
  })
  output$areaBox <- renderValueBox({
    valueBox(
      paste0(length(geo_area)), "Geo. Areas", icon = icon("list"),
      color = "purple"
    )
  })
  output$populationBox <-  renderValueBox({
    valueBox(
      paste0(length(population)), "Population Served", icon = icon("list"),
      color = "purple"
    )
  })
  output$grantBox <-  renderValueBox({
    valueBox(
      paste0(length(grant_program)), "Grant Programs", icon = icon("list"),
      color = "purple"
    )
  })
  output$programBox <-  renderValueBox({
    valueBox(
      paste0(length(program_area)), "Program Areas", icon = icon("list"),
      color = "purple"
    )
  })
  output$budgetBox <-  renderValueBox({
    valueBox(
      paste0(length(budget_fund)), "Budget Fund Areas", icon = icon("list"),
      color = "purple"
    )
  })
  
#--------------
# Grants across fiscal Year
#--------------
  output$grantPlot <- renderPlot({
    grant_df <- df %>%
      group_by(Fiscal_year_update) %>%
      summarise(grant_total = sum(Amount_awarded)) %>%
      select(Fiscal_year_update, grant_total)
    
    ggplot(grant_df, aes(as.factor(Fiscal_year_update),grant_total)) + 
      geom_bar(stat="identity", width = 0.5, fill="#bc5090") +
      theme_minimal() + scale_y_continuous(labels = comma) +
      labs(x = "Year", y = "Total Grant Amount") + 
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  
#===============
# City Insights
#===============
#----------------
# Age groups
#----------------
    output$ageGroupCityPlot <- renderPlot({
      
      output_df <- df %>%
        filter(Recipient_org_city_update == input$cityInput,
               Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
               Age_group_update != "Not Specified") %>%
        group_by(Age_group_update) %>%
        summarise(grant_total = sum(Amount_awarded)) %>%
        select(Age_group_update, grant_total)
      
      ggplot(output_df, aes(reorder(Age_group_update,grant_total),grant_total)) + 
        geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
        theme_minimal() + scale_y_continuous(labels = comma) +
        labs(x = "Age Group", y = "Grants Total") + 
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 00, hjust = 1)) 
      
    })
#----------------
# Area served
#----------------
output$areaServedCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
           Age_group_update != "Not Specified") %>%
    group_by(Geographical_area_served) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Geographical_area_served, grant_total)
  
  ggplot(output_df, aes(reorder(Geographical_area_served,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Geographical Area", y = "Grants Total", fill="") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})

#----------------
# Population served
#----------------
output$populationCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
           Age_group_update != "Not Specified",
           Population_served != "Not Specified") %>%
    group_by(Population_served) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Population_served, grant_total)
  
  ggplot(output_df, aes(reorder(Population_served,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Population Served", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})

#----------------    
# of grant programs
#----------------
output$grantProgramCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Grant_program) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Grant_program, grant_total)
  
  ggplot(output_df, aes(reorder(Grant_program,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Grant program", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})

#----------------
# Program area
#----------------  
output$programAreaCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Program_area_update) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Program_area_update, grant_total)
  
  ggplot(output_df, aes(reorder(Program_area_update,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Program Area", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})
  
#----------------
# Budget fund
#----------------
output$budgetCityPlot<- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Budget_fund_update) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Budget_fund_update, grant_total)
  
  ggplot(output_df, aes(reorder(Budget_fund_update,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Budget Area", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})

#----------------
# Organization
#----------------
output$organizationCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Organization_name) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Organization_name, grant_total)
  
  ggplot(output_df, aes(reorder(Organization_name,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="light green") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Organization", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})

#----------------  
# Grants across fiscal Year (Amount applied)
#----------------
output$grantCityPlot <- renderPlot({
  output_df <- df %>%
    filter(Recipient_org_city_update == input$cityInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Fiscal_year_update) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Fiscal_year_update, grant_total)
  
  ggplot(output_df, aes(as.factor(Fiscal_year_update),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="blue") + 
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
  
})
  
#===============
# Organization Insights
#===============
#--------------
# Age groups
#--------------
output$ageGroupOrgPlot <- renderPlot({
  output_df <- df %>%
    filter(Organization_name == input$organizationInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
           Age_group_update != "Not Specified") %>%
    group_by(Age_group_update) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Age_group_update, grant_total)
  
  ggplot(output_df, aes(reorder(Age_group_update,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Age Group", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})
#--------------
# Area served
#--------------
output$areaServedOrgPlot <- renderPlot({
  output_df <- df %>%
    filter(Organization_name  == input$organizationInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
           Age_group_update != "Not Specified") %>%
    group_by(Geographical_area_served) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Geographical_area_served, grant_total)
  
  ggplot(output_df, aes(reorder(Geographical_area_served,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Geographical Area", y = "Grants Total", fill="") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
  
})
#--------------
# Population served
#--------------
output$populationOrgPlot <- renderPlot({
  output_df <- df %>%
    filter(Organization_name == input$organizationInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2],
           Age_group_update != "Not Specified",
           Population_served != "Not Specified") %>%
    group_by(Population_served) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Population_served, grant_total)
  
  ggplot(output_df, aes(reorder(Population_served,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Population Served", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})
#--------------
# of grant programs
#--------------
output$grantProgramOrgPlot <- renderPlot({
  output_df <- df %>%
    filter(Organization_name == input$organizationInput,
           Fiscal_year_update >= input$yearInput[1] & Fiscal_year_update <= input$yearInput[2]) %>%
    group_by(Grant_program) %>%
    summarise(grant_total = sum(Amount_awarded)) %>%
    select(Grant_program, grant_total)
  
  ggplot(output_df, aes(reorder(Grant_program,grant_total),grant_total)) + 
    geom_bar(stat="identity", width = 0.5, fill="#bc5090") + coord_flip() +
    theme_minimal() + scale_y_continuous(labels = comma) +
    labs(x = "Grant program", y = "Grants Total") + 
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 00, hjust = 1)) 
})
#--------------
# Program area
#--------------
output$programAreaOrgPlot <- renderPlot({})
#--------------
# Budget fund
#--------------
output$budgetOrgPlot<- renderPlot({})
#--------------
# City Insight
#--------------
output$cityOrgPlot <- renderPlot({})
#--------------
# Grants across fiscal Year (Amount applied)
#--------------

#--------------  
# top words
#--------------
  
#--------------
# Average sentiment of descriptions
#--------------
  
}

shinyApp(ui, server)