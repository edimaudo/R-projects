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
                            selectInput("ProductInput", "Product", choices = product_info, 
                                        selected="B000GG87V2"),
                            selectInput("CountryInput", "Country", choices = country_info,
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
                            plotOutput("sentimentPlot"),
                            plotOutput("termFrequencyPlot"),
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

# Define server logic 
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
        
    })
    
    #==============
    #Term frequency
    #==============
    termFrequencyPlot <- renderPlot({
        
    })
    #==============
    # Topic modeling
    #==============
    output$topicTable <- DT::renderDataTable({
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
        
        #DT::datatable(top_terms,options = list(scrollX = TRUE))
        top_terms
        DT::datatable(output_df,options = list(pageLength = 20,scrollX = TRUE,scrollY = "500px"))
        
    })
    
}


shinyApp(ui, server)