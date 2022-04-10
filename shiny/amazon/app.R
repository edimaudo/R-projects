################
# Packages
################
rm(list = ls()) #clear environment
packages <- c('ggplot2','corrplot','tidyverse','readxl', 
              'RColorBrewer','shiny','shinydashboard','scales','dplyr',
              'forecast','lubridate','stopwords','tidytext','stringr',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}
################
# Load data
################
df <- read.csv("Amazon_Reviews_Vitamin_C.csv")

#=============
# Text analytics
#=============
# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("won't", "will not", doc)
    doc <- gsub("can't", "can not", doc)
    doc <- gsub("n't", " not", doc)
    doc <- gsub("'ll", " will", doc)
    doc <- gsub("'re", " are", doc)
    doc <- gsub("'ve", " have", doc)
    doc <- gsub("'m", " am", doc)
    doc <- gsub("'d", " would", doc)
    # 's could be 'is' or could be possessive: it has no expansion
    doc <- gsub("'s", "", doc)
    return(doc)
}

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# fix (expand) contractions
df$Review <- sapply(df$Review, fix.contractions)
# remove special characters
df$Review <- sapply(df$Review, removeSpecialChars)
# convert everything to lower case
df$Review <- sapply(df$Review, tolower)
remove_keywords <- c("printer","print", "printing","app")

textcleaner <- function(x){
    x <- as.character(x)
    
    x <- x %>%
        str_to_lower() %>%  # convert all the string to low alphabet
        replace_contraction() %>% # replace contraction to their multi-word forms
        replace_internet_slang() %>% # replace internet slang to normal words
        #replace_emoji(replacement = " ") %>% # replace emoji to words
        #replace_emoticon(replacement = " ") %>% # replace emoticon to words
        replace_hash(replacement = "") %>% # remove hashtag
        replace_word_elongation() %>% # replace informal writing with known semantic replacements
        replace_number(remove = T) %>% # remove number
        replace_date(replacement = "") %>% # remove date
        replace_time(replacement = "") %>% # remove time
        str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
        str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
        str_squish() %>% # reduces repeated whitespace inside a string.
        str_trim() # removes whitespace from start and end of string
    
    return(as.data.frame(x))
    
}




################
# UI
################
ui <- dashboardPage(
    dashboardHeader(title = "Amazon Review Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("th")),
            menuItem("Term Frequency", tabName = "term", icon = icon("th")),
            menuItem("Topic Modeling", tabName = "topic", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #===============
            # Summary
            #===============
            tabItem(tabName = "summary",
                        mainPanel(
                            h1("Summart",style="text-align: center;"), 
                            # tabsetPanel(type = "tabs",
                            #             tabPanel(h4("Average Printer Score",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreplot")),
                            #             tabPanel(h4("Printer Score Count",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("countPrinterScoreplot")),
                            #             tabPanel(h4("Average Printer Score over time",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreYearplot"))
                            # )
                        )
                    ),
                    
            
            #==============
            #Analysis UI
            #==============
            tabItem(tabName = "analysis",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Analysis",style="text-align: center;"), 
                            # tabsetPanel(type = "tabs",
                            #             tabPanel(h4("Average Printer Score",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreplot")),
                            #             tabPanel(h4("Printer Score Count",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("countPrinterScoreplot")),
                            #             tabPanel(h4("Average Printer Score over time",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreYearplot"))
                            # )
                        )
                    )
            ), 
            #==============
            # Sentiment Analysis UI
            #==============
            tabItem(tabName = "sentiment",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            # checkboxGroupInput("ratingInput", "Ratings",
                            #                    choices = score_info, 
                            #                    selected = score_info),
                            # submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Sentiment Analysis",style="text-align: center;"),
                            plotOutput("sentimentplot")
                        )
                    )
            ),
            #==============
            # Term frequency
            #==============
            tabItem(tabName = "term",
                    #sidebarLayout(
                    #sidebarPanel(
                    #checkboxGroupInput("printerInput", "Printers",
                    #                   choices = printer_info, 
                    #                   selected = printer_info),
                    #submitButton("Submit")
                    # ),
                    mainPanel(
                        h1("TF-IDF",style="text-align: center;"), 
                        plotOutput("termplot")
                        
                    )
                    # )
            ),
            #==============
            # Topic modeling
            #==============
            tabItem(tabName = "topic",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            # checkboxGroupInput("ratingInput", "Ratings",
                            #                    choices = score_info, 
                            #                    selected = score_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Topic Modeling",style="text-align: center;"),
                            DT::dataTableOutput("termtable")
                        )
                    )
            )
        )
    )
    
)

################
# Server
################

# Define server logic 
server <- function(input, output,session) {
    
}


shinyApp(ui, server)