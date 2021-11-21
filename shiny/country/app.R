# GDP and Country analysis

rm(list = ls()) #clear environment

#===============
# libraries
#===============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','scales',
              'dplyr','mlbench','caTools','forecast','TTR','xts','lubridate','shiny',
              'shinydashboard')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#===============
# Load data
#===============
gdp <- read.delim("GDP.txt",header = TRUE)
population <- read.delim("Population.txt",header = TRUE, sep = ",")

#===============
# Data cleaning
#===============
colnames(gdp) <- c("Country","Country_code","Year","GDP_value")
colnames(population) <- c("Country","Country_code","Year","Population_value")

# Country
gdp_country_info <- as.vector(unique(gdp$Country)) 
population_country_info <- as.vector(unique(population$Country)) 
combined_country_info <- c(gdp_info,population_info)
country_info <- as.vector(unique(combined_country_info))

# Year
gdp_year_info <- as.vector(unique(gdp$Year)) 
population_year_info <- as.vector(unique(population$Year)) 
combined_year_info <- c(gdp_year_info,population_year_info)

#===============
# Drop downs
#===============
country_1<- as.vector(unique(combined_country_info))
country_2<- as.vector(unique(combined_country_info))
year_info <- as.vector(unique(combined_year_info))

#===============
# Define UI 
#===============
ui <- dashboardPage(
    dashboardHeader(title = "GDP and Population Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("GDP/Population", tabName = "gdp_population", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            
            tabItem(tabName = "gdp_population",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("countryInput1", "Country Information", 
                                        choices = country_1),
                            selectInput("countryInput2", "Country Information 2", 
                                        choices = country_2),
                            sliderInput("YearInput", "Years", min = 1960, max = 2018, value = 2018,
                                        step=1, ticks = FALSE, sep=""),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h2("GDP and Population",style="text-align: center;"), 
                            fluidRow(
                                h3("GDP Plot",style="text-align: center;"),
                                #plotOutput("forecastPlot"),
                                br(),
                                h3("Population Plot",style="text-align: center;"),
                                #plotOutput("forecastPlot"),
                            )
                        )
                    )
            ) 
        )
    )
)

#===============
# Define server logic 
#===============
server <- function(input, output,session) {

    }

shinyApp(ui, server)
