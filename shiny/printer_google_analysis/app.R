# User content sentiment analysis
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
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
# data cleaning
#=============
df[df==0] <- NA #assigne 0 to NA
df <- na.omit(df) #remove na

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
                            checkboxGroupInput("printerInput", "Printer",choices = printer_info, 
                                               selected = printer_info),
                            # selectInput("horizonInput", "Horizon", 
                            #             choices = horizon_info, selected = 14),
                            # selectInput("frequencyInput", "Frequency", 
                            #             choices = frequency_info, selected = 7),
                            # radioButtons("differenceInput","Difference",
                            #              choices = difference_info, selected = "No"),
                            # radioButtons("logInput","Log",
                            #              choices = log_info, selected = "No"),
                            # selectInput("modelInput", "Model", 
                            #             choices = model_info, selected = 'auto exponential'),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Analysis",style="text-align: center;"), 
                            tabsetPanel(type = "tabs",
                                         tabPanel(h4("Avg Printer Score",style="text-align: center;"), 
                                                  plotOutput("avgPrinterScoreplot")),
                                         tabPanel(h4("ACF Plot",style="text-align: center;"), 
                                                  plotOutput("")),
                                         tabPanel(h4("PACF Plot",style="text-align: center;"), 
                                                  plotOutput("")),
                                         tabPanel(h4("Forecast Output",style="text-align: center;"), 
                                                  plotOutput("")),
                                         tabPanel(h4("Forecast Accuracy",style="text-align: center;"), 
                                                  plotOutput(""))
                            )
                        )
                    )
            ) 
        )
    )
)

# Define server logic 
server <- function(input, output,session) {
    output$avgPrinterScoreplot <- renderPlot({
        
        printer_selection <- unlist(strsplit(input$printerInput, split=" "))
        printer_selection <- c(printer_selection)
        print(printer_selection)
        df_avg_score <- df %>%
            group_by(printer) %>%
            filter(printer %in% printer_selection) %>%
            summarise(score_avg = mean(score), score_count = n()) %>%
            select(printer,score_avg, score_count) 
        
            ggplot(df_avg_score, aes(x = reorder(printer,score_avg), y = score_avg)) + 
            geom_bar(stat = "identity", width = 0.3) + theme_light()  + 
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
        
        
        # ggplot(data = data_df,aes(x = as.factor(year),y = total_pledges)) +
        #     geom_bar(stat = "identity", width = 0.3) + theme_light() +
        #     labs(x = "Years",
        #          y = "Total # of Pledges") +
        #     scale_y_continuous(labels = comma) +
        #     scale_x_discrete() +

            
            # data_df <- df %>%
            #     filter(city == input$cityInput) %>%
            #     filter(major_category == input$categoryInput) %>%
            #     group_by(year) %>%
            #     summarise(total_pledges = sum(amt_pledged_.))
            # 
            # ggplot(data = data_df,aes(x = as.factor(year),y = total_pledges)) +
            #     geom_bar(stat = "identity", width = 0.3) + theme_light() +
            #     labs(x = "Years",
            #          y = "Amount Pledged ($)") +
            #     scale_y_continuous(labels = comma) +
            #     scale_x_discrete() +
            #     theme(
            #         legend.text = element_text(size = 10),
            #         legend.title = element_text(size = 10),
            #         axis.title = element_text(size = 15),
            #         axis.text = element_text(size = 10),
            #         axis.text.x = element_text(angle = 45, hjust = 1)
                #)
    })
    
}

shinyApp(ui, server)