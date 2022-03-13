# User content sentiment analysis
rm(list = ls()) #clear environment

#=============
# libraries
#=============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# load data
#=============
df <- read.csv("reviews_v2.csv")

#=============
# Data cleaning
#=============
df$at <- as.Date(df$at, format = "%m/%d/%Y")
df$year <- lubridate::year(df$at)
df$month <- lubridate::month((df$at))


#=============
# Add printer column
#=============
df$printer <- ifelse(df$appId == "com.hp.printercontrol", 'HP',
                     ifelse(df$appId == "jp.co.canon.bsd.ad.pixmaprint", 'Canon',
                            ifelse(df$appId == "epson.print", 'Epson', 'Epson-Smart')))

#=============
# Dropdown information
#=============                        
printer_info <- c('Canon','Epson','Epson-Smart','HP')

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Printer Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("printerInput", "Printers",
                                               choices = printer_info, 
                                               selected = printer_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Simple Stats.",style="text-align: center;"), 
                            tabsetPanel(type = "tabs",
                                    tabPanel(h4("Printer Score Average",style="text-align: center;"), 
                                        plotOutput("avgPrinterScoreplot")),
                                    tabPanel(h4("Printer Score Count",style="text-align: center;"), 
                                        plotOutput("countPrinterScoreplot")),
                                    tabPanel(h4("Printer Score Average over time",style="text-align: center;"), 
                                         plotOutput("avgPrinterScoreYearplot"))
                            )
                        )
                    )
            ), tabItem(tabName = "sentiment")
        )
    )
)

# sentiment analysis
# topic modeling
# TF-IDF

# Define server logic 
server <- function(input, output,session) {
   
    output$avgPrinterScoreplot <- renderPlot({
        
        if (is.null(input$printerInput)){
        
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            printer_selection <- c(printer_selection)
            df_avg_score <- df %>%
                group_by(printer) %>%
                filter(printer %in% printer_selection) %>%
                summarise(score_avg = mean(score), score_count = n()) %>%
                select(printer,score_avg, score_count) 
            
            ggplot(df_avg_score, aes(x = reorder(printer,score_avg), y = score_avg)) + 
                geom_bar(stat = "identity", width = 0.3, fill = "#FF6566") + theme_light()  + 
                coord_flip() + 
                guides(fill = FALSE) + 
                ggtitle("Average score of Printers") + 
                xlab("Printer") + 
                ylab("Average. Score") + 
                theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.text = element_text(size = 20),
                    legend.title = element_text(size = 25),
                    axis.title = element_text(size = 15),
                    axis.text = element_text(size = 15)
                )                
        }
    })
    
    output$countPrinterScoreplot <- renderPlot({
        
        if (is.null(input$printerInput)){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            printer_selection <- c(printer_selection)
            df_avg_score <- df %>%
                group_by(printer) %>%
                filter(printer %in% printer_selection) %>%
                summarise(score_avg = mean(score), score_count = n()) %>%
                select(printer,score_avg, score_count) 
            
            ggplot(df_avg_score, aes(x = reorder(printer,score_count), y = score_count)) + 
                geom_bar(stat = "identity", width = 0.3, fill = "#AA6566") + theme_light()  + 
                coord_flip() + 
                guides(fill = FALSE) + 
                ggtitle("Score Count of Printers") + 
                xlab("Printer") + 
                ylab("Score Count") + 
                theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.text = element_text(size = 20),
                    legend.title = element_text(size = 25),
                    axis.title = element_text(size = 15),
                    axis.text = element_text(size = 15)
                )              
        }
        
    })
    
    output$avgPrinterScoreYearplot <- renderPlot({
        if (is.null(input$printerInput)){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            printer_selection <- c(printer_selection)
            
            df_score <- df %>%
                group_by(printer, year) %>%
                filter(printer %in% printer_selection) %>%
                summarise(score_avg = mean(score), score_count = n()) %>%
                select(printer,year, score_avg, score_count)
            
            ggplot(df_score, aes(x = year, y = score_avg, fill=printer)) + 
                geom_bar(stat = "identity", position = "dodge", width = 0.3) + theme_light()  + 
                coord_flip() + 
                # 
                ggtitle("Score Average of Printers by Year") + 
                 xlab("Year") + 
                ylab("Score Average") #+ 
                # theme(legend.position="bottom") +
                # theme(
                #     plot.title = element_text(hjust = 0.5),
                #     legend.text = element_text(size = 10),
                #     legend.title = element_text(size = 10),
                #     axis.title = element_text(size = 15),
                #     axis.text = element_text(size = 15), 
                #     
                # )
        }
    })
    
    output$countPrinterScoreYearplot <- renderPlot({
        
        if (is.null(input$printerInput)){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            printer_selection <- c(printer_selection)
            
            df_score <- df %>%
                group_by(printer, year) %>%
                filter(printer %in% printer_selection) %>%
                summarise(score_avg = mean(score), score_count = n()) %>%
                select(printer,year, score_avg, score_count)
            
            ggplot(df_score, aes(x = reorder(year, score_count), y = score_count, fill=printer)) + 
                geom_bar(stat = "identity", width = 0.3,position = "dodge") + theme_light()  + 
                coord_flip() + 
                
                ggtitle("Score count of Printers by Year") + 
                xlab("Year") + 
                ylab("Score Count") + 
                theme(legend.position="bottom") +
                theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.text = element_text(size = 10),
                    legend.title = element_text(size = 10),
                    axis.title = element_text(size = 15),
                    axis.text = element_text(size = 15), 
                    
                )
        
        }
        
    })
    
}

shinyApp(ui, server)