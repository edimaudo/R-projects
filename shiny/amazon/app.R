################
# Packages
################
packages <- c('ggplot2','corrplot','tidyverse','readxl','DT',
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
months <- c("January","February","March","April",'May',"June","July","August","September","October",
            "November","December")
#=============
# Text analytics
#=============


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
        #replace_time(replacement = "") %>% # remove time
        str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
        str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
        str_squish() %>% # reduces repeated whitespace inside a string.
        str_trim() # removes whitespace from start and end of string
    
    return(as.data.frame(x))
    
}

################
# UI
################

#----------------
#UI drop-downs
#----------------
product_info <- sort(unique(df$Procuct))
country_info <- sort(unique(df$Country))
month_info <- sort(unique(df$Month))
year_info <- sort(unique(df$Year))
rating_info <- sort(unique(df$Rating))

#----------------
#UI design
#----------------
ui <- dashboardPage(
    dashboardHeader(title = "Amazon Review Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("clock")),
            menuItem("Explore", tabName = "explore", icon = icon("th")),
            menuItem("Text Analysis", tabName = "text", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #===============
            # Summary
            #===============
            tabItem(tabName = "summary",
                        mainPanel(
                            h1("Summary",style="text-align: center;"), 
                            fluidRow(
                                valueBoxOutput("productBox", width = 3),
                                valueBoxOutput("countryBox",width = 3),
                                valueBoxOutput("yearBox",width = 3),
                                valueBoxOutput("ratingBox",width = 3),
                            ),
                            
                             tabsetPanel(type = "tabs",
                                         tabPanel(h4("Avg. Rating by Month",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgMonthRatingPlot")),
                                         tabPanel(h4("Average Rating by Year",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgYearRatingPlot"))
                             )
                        )
                    ),
                    
            
            #==============
            #Explore UI
            #==============
            tabItem(tabName = "explore",
                        mainPanel(
                            h1("Data Exploration",style="text-align: center;"),
                            DT::dataTableOutput("exploreTable")
                        )
            ), 
            #==============
            # Text analytics UI
            #==============
            tabItem(tabName = "text",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("productInput", "Product", choices = product_info, 
                                        selected="B000GG87V2"),
                            selectInput("countryInput", "Country", choices = country_info,
                                        selected="United States "),
                            selectInput("monthInput", "Month", choices = month_info, 
                                        selected="January"),
                            selectInput("ratingInput", "Rating", choices = rating_info,
                                        selected="5"),
                            sliderInput("yearInput","Year",min=min(year_info),max=max(year_info),
                                        value = c(min(year_info),
                                                  max(year_info)),step =1,ticks = FALSE),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Text Analysis",style="text-align: center;"),
                            #plotOutput("sentimentPlot"),
                            #plotOutput("termFrequencyPlot"),
                            DT::dataTableOutput("topicTable")
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
    # Summary logic
    #===============
    #--------------
    # Tab-boxes
    #--------------
    output$productBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Procuct))), "Products", icon = icon("list"),
            color = "green"
        )
    })
    
    output$countryBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Country))), "Countries", icon = icon("list"),
            color = "green"
        )
    })
    
    output$yearBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Year))), "Years", icon = icon("list"),
            color = "green"
        )
    })
    
    output$ratingBox <- renderValueBox({
        output <- mean(na.omit(df$Rating))
        output <- format(round(output, 1), nsmall = 2)
        valueBox(
            paste0(output), "Avg. Ratings", icon = icon("list"),
            color = "green"
        )
    })
    
    #--------------
    # Ratings plot
    #--------------
    output$avgMonthRatingPlot <- renderPlot({
        output_df <- df %>%
            mutate(Month = factor(Month, levels = months)) %>%
            group_by(Month) %>%
            summarise(avg_rating = mean(na.omit(Rating))) %>%
            select(Month, avg_rating)
        output_df <- na.omit(output_df)
        
        ggplot(output_df, aes(as.factor(Month),avg_rating)) + 
            geom_bar(stat="identity", width = 0.5, fill="#bc5090") + 
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Month", y = "Average Rating") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1)) 
        
    })
    
    output$avgYearRatingPlot <- renderPlot({
        output_df <- df %>%
            group_by(Year) %>%
            summarise(avg_rating = mean(na.omit(Rating))) %>%
            select(Year, avg_rating)
        output_df <- na.omit(output_df)
        
        ggplot(output_df, aes(as.factor(Year),avg_rating)) + 
            geom_bar(stat="identity", width = 0.5, fill="#bc5090") + 
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Year", y = "Average Rating") + 
            theme(legend.text = element_text(size = 13),
                  legend.title = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  axis.text = element_text(size = 13),
                  axis.text.x = element_text(angle = 00, hjust = 1)) 
    })
    
    #==============
    #Explore logic
    #==============
    output$exploreTable <- DT::renderDataTable({
        output_df <- df %>%
            dplyr::select(Procuct,Country, Month, Year, Title, Body, Rating)
        DT::datatable(output_df,options = list(pageLength = 20,scrollX = TRUE,scrollY = "500px"))
        
    })
    #==============
    # Text analytics logic
    #==============
    
    #==============
    # Sentiment analysis
    #==============
    output$sentimentPlot <- renderPlot({
        # word breakdown 
        review_words <- df %>%
            unnest_tokens(word, Body) %>%
            anti_join(stop_words) %>%
            distinct() %>%
            filter(nchar(word) > 3) 
        
        bing_word_counts <- review_words %>%
            filter(input$productInput == Procuct, 
                   input$countryInput == Country,
                   input$monthInput == Month,
                   input$ratingInput == Country,
                   input$yearInput == Year) %>%
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
    })
    
    #==============
    #Term frequency
    #==============
    output$termFrequencyPlot <- renderPlot({
        
        # tf-idf by Rating & Product
        popular_tfidf_words <- df %>%
            unnest_tokens(word, Body) %>%
            distinct() %>%
            filter(nchar(word) > 3,
                   input$productInput == Procuct, 
                   input$countryInput == Country,
                   input$monthInput == Month,
                   input$ratingInput == Country,
                   input$yearInput == Year) %>%
            count(Rating, Procuct,word, sort = TRUE) %>%
            ungroup() %>%
            bind_tf_idf(word, Rating, n)
        
        top_popular_tfidf_words <- popular_tfidf_words %>%
            arrange(desc(tf_idf)) %>%
            mutate(word = factor(word, levels = rev(unique(word)))) %>%
            group_by(Procuct, Rating) %>% 
            slice(seq_len(8)) %>%
            ungroup() %>%
            arrange(desc(Procuct, Rating)) %>%
            mutate(row = row_number())
        
        #td-idf by Product
        top_popular_tfidf_words %>%
            ggplot(aes(x = row, tf_idf, 
                       fill = Procuct)) +
            geom_col(show.legend = NULL) +
            labs(x = NULL, y = "TF-IDF") + 
            ggtitle("Important Words using TF-IDF by Product") +
            theme_bw() +  
            facet_wrap(~Procuct, ncol = 6, scales = "free") +
            scale_x_continuous(  # This handles replacement of row 
                breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
                labels = top_popular_tfidf_words$word) +
            coord_flip()
        
    })
    #==============
    # Topic modeling
    #==============
    output$topicTable <- DT::renderDataTable({
        set.seed(1502)
        df <- df %>%
            filter(input$productInput == Procuct, 
                   input$countryInput == Country,
                   input$monthInput == Month,
                   input$ratingInput == Country,
                   input$yearInput == Year)
        
        clean <- textcleaner(df$Body)
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
        
        DT::datatable(top_terms,options = list(pageLength = 10,scrollX = TRUE,scrollY = "500px"))
        
    })
    
}


shinyApp(ui, server)