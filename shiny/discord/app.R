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
  'data.table','scales'
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
df$quarter <- paste0("Quarter","-",quarter(df$Date2))
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
                    plotOutput("yearlyTrendPlot"),
                  ),
                  fluidRow(
                    h4("Quarterly Trend",style="text-align: center;"),
                    plotOutput("quarterlyTrendPlot"),
                  ),
                  fluidRow(
                    h4("Monthly Trend",style="text-align: center;"),
                    plotOutput("monthlyTrendPlot"),
                    
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
  #-------------
  # Trend Output
  #-------------
  output$yearlyTrendPlot <- renderPlot({
    
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,year) %>%
      summarize(total_count=n()) %>%
      select(channel_category,year,total_count) %>%
      ggplot( aes(x=year, y=total_count, group=channel_category, color=channel_category)) +
      geom_line() + theme_classic() + 
      labs(x ="Year", y = "# of Messages",color='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))  
  
  })

  output$quarterlyTrendPlot <- renderPlot({
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,quarter) %>%
      summarize(total_count=n()) %>%
      select(channel_category,quarter,total_count) %>%
      ggplot(aes(x = quarter ,y = total_count, fill=channel_category))  +
      geom_bar(stat = "identity",width = 0.5) + theme_classic() + 
      labs(x ="Quarter", y = "# of Messages",fill='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
    
  })
  output$monthlyTrendPlot <- renderPlot({
  
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,month) %>%
      summarize(total_count=n()) %>%
      select(channel_category,month,total_count) %>%
      ggplot( aes(x=month, y=total_count, group=channel_category, color=channel_category)) +
      geom_line() + theme_classic() + 
      labs(x ="Month", y = "# of Messages",color='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(hjust=0, angle=30))  
    
  })

  output$weeklyTrendPlot <- renderPlot({
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,week) %>%
      summarize(total_count=n()) %>%
      select(channel_category,week,total_count) %>%
      ggplot( aes(x=week, y=total_count, group=channel_category, color=channel_category)) +
      geom_line() + theme_classic() + 
      labs(x ="Week", y = "# of Messages",color='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))  
    
  })

  output$dailyTrendPlot <- renderPlot({
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,day) %>%
      summarize(total_count=n()) %>%
      select(channel_category,day,total_count) %>%
      ggplot( aes(x=day, y=total_count, group=channel_category, color=channel_category)) +
      geom_line() + theme_classic() + 
      labs(x ="Day", y = "# of Messages",color='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))  
  })
  
  output$dowTrendPlot <- renderPlot({
    df %>% 
      filter(channel_category %in% input$channelInput ) %>%
      group_by(channel_category,dayofweek) %>%
      summarize(total_count=n()) %>%
      select(channel_category,dayofweek,total_count) %>%
      ggplot(aes(x = dayofweek ,y = total_count, fill=channel_category))  +
      geom_bar(stat = "identity",width = 0.5) + theme_classic() + 
      labs(x ="Day of Week", y = "# of Messages",fill='Channels') +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
 
   
#2) Community Activity
#channels by various metrics (messages, attachments sent, reactions received). 
#Analyze the day of week and time of day for activity 

#-dropdown (channel)
#  --day of week chart by user activity volume  

  
  # 3) Server Activity Prediction Model
  # - channel
  # -Develop forecasting models to predict future server activity, 
  #  day + content posted  --> server activity   
}




shinyApp(ui, server)