library(shinydashboard)
library(shiny)
library(DT)
library(wordcloud)
library(rAmCharts)
library(plyr)

file_path <- "movie.csv"
movie <- read.csv(file_path, stringsAsFactors = FALSE)
movie <- movie[complete.cases(movie), ]
genres <- strsplit(movie$Genre, ",")
genres <- unlist(genres)
genres <- unique(genres)
genre_choices <- append(genres, "Select All", after = 0)
directors_choices <- unique(movie$Director)
directors_choices <- append(directors_choices, "Select All", after = 0)
actors <- strsplit(movie$Actors, ",")
actors <- unlist(actors)
actors <- unique(actors)
actors_choices <- append(actors, "Select All", after = 0)

header <- dashboardHeader(title = "The Movie App") 
sidebar <- dashboardSidebar(
  sidebarMenu(
    img(src = "https://thumbs.dreamstime.com/b/movie-icon-items-cinema-white-background-35291742.jpg", height = 240, width = 230),
    sliderInput("Rating", "Movie according to the rating",0,10,c(0,10)),
    sliderInput("Year", "Year released", 2006, 2016, c(2006, 2016)),
    selectInput("genre_input", "Genre (a movie can have multiple genres)",genre_choices, selected = "Select All"),
    selectInput("directors_input","Director",directors_choices, selected = "Select All"),
    selectInput("actors_input","Actor",actors_choices, selected = "Select All")
  )
)


page <- dashboardBody(
  fluidRow(
    box(width = 6, amChartsOutput("top_box_office_collections"), title = "Top 10 Box Office Collections"), 
    box(width = 6, amChartsOutput("top_imdb_ratings"), title = "Top 10 IMDB Ratings")
  ),
  fluidRow(
    box(width = 6, plotOutput("actors_wordcloud"), title = "Active Actors"), 
    box(width = 6, amChartsOutput("year_on_year_movies"), title = "Year On Year Movies")
  ),
  fluidRow(
    box(width = 12, DT::DTOutput("movies_table"), title = "All Filtered Movies")
  )
)

ui<-dashboardPage(header,sidebar,page,skin='red')

server<-(function(input, output) {
  
  filtered_data <- reactive({
    my_data <- movie
    my_data <- my_data[(my_data$Year>=input$Year[1]) & (my_data$Year<=input$Year[2]), ]
    my_data <- my_data[(my_data$Rating>=input$Rating[1]) & (my_data$Rating<=input$Rating[2]), ]
    if (input$genre_input != "Select All") { 
      my_data <- my_data[grepl(input$genre_input, my_data$Genre), ]
    } 
    if (input$directors_input != "Select All") { 
      my_data <- my_data[grepl(input$directors_input,my_data$Director),]
    }
    if (input$actors_input != "Select All") { 
      my_data <- my_data[grepl(input$actors_input, my_data$Actors), ]
    }
    my_data
  })
  
  output$movies_table <- renderDT({ 
    filtered_movies_table <- filtered_data()
    filtered_movies_table <- filtered_movies_table[, c("Title", "Genre", "Description", "Director", "Actors", "Rating")]
    DT::datatable(filtered_movies_table)
    
  })
  
  output$top_box_office_collections <- renderAmCharts({
    box_office_collections_plot <- filtered_data()
    box_office_collections_plot <- box_office_collections_plot[order(box_office_collections_plot$Revenue_in_Millions, decreasing = T), ]
    box_office_collections_plot <- head(box_office_collections_plot, 10)
    amBarplot(x = "Title", y = "Revenue_in_Millions", data = box_office_collections_plot,
              labelRotation = -45) 
  })
  
  output$top_imdb_ratings <- renderAmCharts({
    top_imdb_ratings_plot <- filtered_data()
    top_imdb_ratings_plot <- top_imdb_ratings_plot[order(top_imdb_ratings_plot$Rating, decreasing = T), ]
    top_imdb_ratings_plot <- head(top_imdb_ratings_plot, 10)
    amBarplot(x = "Title", y = "Rating", data = top_imdb_ratings_plot,
              labelRotation = -45)
  })
  
  output$actors_wordcloud <- renderPlot({
    actors_wordcloud_plot <- filtered_data()
    actors_wordcloud_plot <- strsplit(actors_wordcloud_plot$Actors, ",")
    actors_wordcloud_plot <- unlist(actors_wordcloud_plot)
    actors_wordcloud_plot <- gsub(" ", "", actors_wordcloud_plot)
    wordcloud(actors_wordcloud_plot)
  })
  
  output$year_on_year_movies <- renderAmCharts({
    year_on_year_movies <- filtered_data()
    year_on_year_movies <- as.data.frame(table(year_on_year_movies$Year))
    colnames(year_on_year_movies) <- c("Year", "Movies")
    amBarplot(x = "Year", y = "Movies", data = year_on_year_movies,
              labelRotation = -45)
  })
  
})

shinyApp(ui,server)
