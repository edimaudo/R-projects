# Printer Review analysis
rm(list = ls()) #clear environment

#=============
# Libraries
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
# Load data
#=============
df <- read.csv("reviews_v2.csv")

#=============
# Data cleaning
#=============
df$at <- as.Date(df$at, format = "%m/%d/%Y")
df$year <- lubridate::year(df$at)
df$month <- lubridate::month((df$at))

#---------------
# Add printer column 
#---------------
df$Product <- ifelse(df$appId == "com.hp.printercontrol", 'HP',
                     ifelse(df$appId == "jp.co.canon.bsd.ad.pixmaprint", 'Canon',
                            ifelse(df$appId == "epson.print", 'Epson', 'Epson-Smart')))

#============= 
# Define UI for application
#============= 

#--------------
# Dropdown information
#--------------
printer_info <- c('Canon','Epson','Epson-Smart','HP')
score_info <- c(1,2,3,4,5)

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

#--------------
# UI
#--------------
ui <- dashboardPage(
    dashboardHeader(title = "Printer Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("th")),
            menuItem("Term Frequency", tabName = "term", icon = icon("th")),
            menuItem("Topic Modeling", tabName = "topic", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",includeMarkdown("readme.md"),hr()),
            #==============
            #Analysis UI
            #==============
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
                                    tabPanel(h4("Average Printer Score",
                                                style="text-align: center;"), 
                                        plotOutput("avgPrinterScoreplot")),
                                    tabPanel(h4("Printer Score Count",
                                                style="text-align: center;"), 
                                        plotOutput("countPrinterScoreplot")),
                                    tabPanel(h4("Average Printer Score over time",
                                                style="text-align: center;"), 
                                         plotOutput("avgPrinterScoreYearplot"))
                            )
                        )
                    )
            ), 
            #==============
            # Sentiment Analysis UI
            #==============
            tabItem(tabName = "sentiment",
                       sidebarLayout(
                           sidebarPanel(
                               checkboxGroupInput("printerInput", "Printers",
                                                  choices = printer_info, 
                                                  selected = printer_info),
                               checkboxGroupInput("ratingInput", "Ratings",
                                                  choices = score_info, 
                                                  selected = score_info),
                               submitButton("Submit")
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
                            checkboxGroupInput("printerInput", "Printers",
                                               choices = printer_info, 
                                               selected = printer_info),
                            checkboxGroupInput("ratingInput", "Ratings",
                                               choices = score_info, 
                                               selected = score_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Topic Modeling",style="text-align: center;"),
                            dataTableOutput("termtable")
                        )
                    )
            )
        )
    )

)


# Define server logic 
server <- function(input, output,session) {
   
    #------------------
    # Average Printer score plot
    #------------------
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
                #ggtitle("Average score of Printers") + 
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
    #------------------
    # Printer Score Count Plot 
    #------------------
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
                #ggtitle("Score Count of Printers") + 
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
    #------------------
    # Average score plot
    #------------------
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
            
            ggplot(df_score, aes(x = year, y = score_avg)) + 
                geom_line(size=2, alpha=1, linetype=1,
                          aes(color = printer, linetype = printer)) +
                geom_point() + 
                theme_light()  + 
                #ggtitle("Average Score of Printers by Year") + 
                xlab("Year") + 
                ylab("Average Score") + 
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 25),
                axis.title = element_text(size = 15),
                axis.text = element_text(size = 15)
            ) 
        }
    })
    #----------------
    # Printer Score over time
    #----------------
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
                
                #ggtitle("Score count of Printers by Year") + 
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
    
    #=================
    # Sentiment Analysis
    #=================
    output$sentimentplot <- renderPlot({

        if (is.null(input$printerInput) | is.null(input$ratingInput)){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            rating_selection <- unlist(strsplit(input$ratingInput, split=" "))
            rating_selection <- c(rating_selection)
            printer_selection <- c(printer_selection)
            
            # word breakdown 
            review_words <- df %>%
                unnest_tokens(word, Review) %>%
                anti_join(stop_words) %>%
                distinct() %>%
                filter(nchar(word) > 3,!word %in% remove_keywords) 
            
            bing_word_counts <- review_words %>%
                filter(Product %in% printer_selection,
                       score %in% rating_selection) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort = TRUE) %>%
                ungroup()
            
            bing_word_counts %>%
                group_by(sentiment) %>%
                top_n(10) %>%
                ggplot(aes(reorder(word, n), n, fill = sentiment)) +
                geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y") +
                labs(y = "Contribution to sentiment", x = NULL) +
                coord_flip()
        
        }    
            
        

                
    })
    

    
    
    #=================
    # Term Frequency
    #=================
    output$termplot <- renderPlot({
        
        if (is.null(input$printerInput)){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            printer_selection <- c(printer_selection)
            NUMBER_COLUMNS = length(printer_selection)
            
            # tf-idf by Product & score
            popular_tfidf_words <- df %>%
                unnest_tokens(word, Review) %>%
                distinct() %>%
                filter(nchar(word) > 3, !word %in% remove_keywords,
                       Product %in% printer_selection) %>%
                count(Product, score, word, sort = TRUE) %>%
                ungroup() %>%
                bind_tf_idf(word, score, n)
            
            top_popular_tfidf_words <- popular_tfidf_words %>%
                arrange(desc(tf_idf)) %>%
                mutate(word = factor(word, levels = rev(unique(word)))) %>%
                group_by(Product, score) %>% 
                slice(seq_len(8)) %>%
                ungroup() %>%
                arrange(desc(Product, score)) %>%
                mutate(row = row_number())
            
            #td-idf by Product
            top_popular_tfidf_words %>%
                ggplot(aes(x = row, tf_idf, 
                           fill = Product)) +
                geom_col(show.legend = NULL) +
                labs(x = NULL, y = "TF-IDF") + 
                #ggtitle("Important Words using TF-IDF by Product") +
                theme_bw() +  
                facet_wrap(~Product, ncol = NUMBER_COLUMNS, scales = "free") +
                scale_x_continuous(  # This handles replacement of row 
                    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
                    labels = top_popular_tfidf_words$word) +
                coord_flip()
            
            
        }
        
    })
    #=================
    # Topic Modeling
    #=================
    output$termtable <- renderDataTable({
        
        if (is.null(input$printerInput) && is.null(input$ratingInput) ){
            
        } else {
            printer_selection <- unlist(strsplit(input$printerInput, split=" "))
            rating_selection <- unlist(strsplit(input$ratingInput, split=" "))
            rating_selection <- c(rating_selection)
            printer_selection <- c(printer_selection)
            
            set.seed(1502)
            df <- df %>%
                filter(score %in% rating_selection, Product %in% printer_selection)
            
            clean <- textcleaner(df$Review)
            clean <- clean %>% mutate(id = rownames(clean))
            
            # crete dtm
            dtm_r <- CreateDtm(doc_vec = clean$x,
                                 doc_names = clean$id,
                                 ngram_window = c(1,2),
                                 stopword_vec = stopwords("en"),
                                 verbose = F)
            
            dtm_r <- dtm_r[,colSums(dtm_r)>2]
            
            mod_lda <- FitLdaModel(dtm = dtm_r,
                                     k = 10, # number of topic
                                     iterations = 500,
                                     burnin = 180,
                                     alpha = 0.1,beta = 0.05,
                                     optimize_alpha = T,
                                     calc_likelihood = T,
                                     calc_coherence = T,
                                     calc_r2 = T)
            
            mod_lda$top_terms <- GetTopTerms(phi = mod_lda$phi,M = 15)
            mod_lda$prevalence <- colSums(mod_lda$theta)/sum(mod_lda$theta)*100
            
            mod_lda$summary <- data.frame(topic = rownames(mod_lda$phi),
                                            coherence = round(mod_lda$coherence,3),
                                            prevalence = round(mod_lda$prevalence,3),
                                            top_terms = apply(mod_lda$top_terms,2,
                                                              function(x){paste(x,collapse = ", ")}))
            
            modsum <- mod_lda$summary %>%
                `rownames<-`(NULL)
            
            top_terms<- modsum %>% 
                arrange(desc(coherence)) %>%
                slice(1:5)
            
            
            
        }
        
    })
    
    
    
}

shinyApp(ui, server)