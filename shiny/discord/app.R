#================================================================================
# Shiny web app which provides insights into Ocean Protocol's Discord server, 
# providing a deeper understanding of the community's dynamics and predicting future activity pattern
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','DT',
  'mlbench','caTools','gridExtra','doParallel','grid','forecast',
  'caret','dummies','mlbench','tidyr','Matrix','lubridate',
  'data.table', 'rsample','scales','plotly'
)
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
df<- read.csv("Ocean Discord Data Challenge Dataset.csv")


################
#Data Setup
################

# channel
df <- df %>%
  mutate(channel_category = case_when(df$Channel %in% c("Ocean Protocol - CORE-TECH - 🌊┃builders [1088751449271447552]",
                                                        "Ocean Protocol - CORE-TECH - 🚀┃tech-announcements [1088412016621932588]",
                                                        "Ocean Protocol - CORE-TECH - 🛳︱github [773926934601269248]") 
                                      ~ 'CORE-TECH',
                                      df$Channel  %in% c("Ocean Protocol - ECOSYSTEM - 🎨︱ai-fun [1088460971191832616]",
                                                         "Ocean Protocol - ECOSYSTEM - 💻┃dev-lounge [971047506857300020]",
                                                         "Ocean Protocol - ECOSYSTEM - 📈︱traders-lounge [1088458691671502890]",
                                                         "Ocean Protocol - ECOSYSTEM - 📊︱data-science-hub [993828971408003152]",
                                                         "Ocean Protocol - ECOSYSTEM - 🚜︱data-farming [993954438790258879]",
                                                         "Ocean Protocol - ECOSYSTEM - 🤖︱predictoor [1151066755796574389]",
                                                         "Ocean Protocol - ECOSYSTEM - 🪢︱ambassadors [769209726415929404]",
                                                         "Ocean Protocol - ECOSYSTEM - 🪢︱treasure-hunter [1113013155514290228]") 
                                      ~ 'ECOSYSTEM',
                                      df$Channel  %in% c("Ocean Protocol - GENERAL - 🎉︱announcements [720631710785732638]",
                                                         "Ocean Protocol - GENERAL - 🐦︱tweets [1162327153128525914]",
                                                         "Ocean Protocol - GENERAL - 💡︱idea-factory [1088534285478269038]",
                                                         "Ocean Protocol - GENERAL - 💭︱general-chat [612953349003673629]",
                                                         "Ocean Protocol - GENERAL - 🗣┃ocean-lounge [1088136164713693258]",
                                                         "Ocean Protocol - GENERAL - 🚨︱report-scammers [912281243012460594]")  
                                      ~ 'GENERAL',
                                      df$Channel  %in% c("Ocean Protocol - GET STARTED - 🌞︱gm [911643560594505809]",
                                                         "Ocean Protocol - GET STARTED - 👋︱welcome [727898674637832274]",
                                                         "Ocean Protocol - GET STARTED - 🖖︱introduce-yourself [747887740263333909]",
                                                         "Ocean Protocol - GET STARTED - 🧐︱ask-the-ai [1082698926865522808]")
                                      ~ 'GET STARTED'))

channel_category_list <- sort(unique(df$channel_category))

#Date setup
df$Date2 <- lubridate::mdy(substring(df$Date,1,10))
df$year <- lubridate::year(df$Date2)
df$quarter <- paste0("Quarter","-",quarter(df$Date2))#paste0(year(df$Date2),"/0",quarter(df$Date2))
df$month <- lubridate::month(df$Date2,label = TRUE,abbr = FALSE)
df$week <- lubridate::week(df$Date2)
df$day <- lubridate::mday(df$Date2)
df$dayofweek <- lubridate::wday(df$Date2, label=TRUE)
df$hour <- lubridate::hour(lubridate::hm(substring(df$Date,12,length(df$Date))))


################
#UI
################
ui <- dashboardPage(
  dashboardHeader(title = "Ocean Discord Data Challenge"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("General Trends", tabName = "general", icon = icon("th")),
      menuItem("Community Activity", tabName = "community", icon = icon("th")),
      menuItem("Server Activity", tabName = "server", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "general",
              sidebarLayout(
                sidebarPanel(
                  helpText("Select a Channel"),
                  checkboxGroupInput("channelInput", "Channels", choices = channel_category_list,
                                     selected=channel_category_list),
                  submitButton("Submit")
                ),
                mainPanel(
                  fluidRow(
                    h4("Yearly Trend",style="text-align: center;"),
                    plotOutput("yearlyTrendePlot"),
                  ),
                  fluidRow(
                    h4("Quarterly Trend",style="text-align: center;"),
                    plotOutput("quarterlyTrendPlot"),
                  ),
                  fluidRow(
                    h4("Monthly Trend",style="text-align: center;"),
                    plotOutput("monthlyTrendPlot"),
                    #plotOutput("monthTrendPlot"),
                  ),
                  fluidRow(
                    h4("Weekly Trend",style="text-align: center;"),
                    plotOutput("weeklyTrendPlot"),
                  ),
                  fluidRow(
                    h4("Daily Trend",style="text-align: center;"),
                    plotOutput("dailyTrendPlot"),
                    plotOutput("dowTrendPlot"),
                  )
                  
                )
              )
      )
    )
   )
  ) 
 

################
# Define server logic 
################
server <- function(input, output,session) {
  
  output$yearlyTrendPlot <- renderPlot({
    
    df_trend <-  df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,year)
      summarise(total_count=count(Content)) %>%
      select(channel_category,year,total_count)
      
    fig <- plot_ly(df_trend, x = ~year, y = ~total_count, type = 'scatter', mode = 'lines',color = ~channel_category)
    fig <- fig %>% layout(title = "",
                            xaxis = list(title = "Year"),
                            yaxis = list(title = "# of Messages"))
    fig
    
  })
  output$quarterlyTrendPlot <- renderPlot({
    
  })
  output$monthlyTrendPlot <- renderPlot({
    
  })

  output$weeklyTrendPlot <- renderPlot({
    
  })

  output$dailyTrendPlot <- renderPlot({
    
  })
  
  output$dowTrendPlot <- renderPlot({
    
  })
  
}




shinyApp(ui, server)