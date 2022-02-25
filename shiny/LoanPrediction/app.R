
rm(list=ls()) #remove old data
#packages
packages <- c("tidyverse",'shiny','shinydashboard','ggplot2', 
              'corrplot','caret','mice', 'caTools',
              'dummies','cluster','factoextra','psy','lattice',
              'nFactors','scales','NbClust')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

options <- c(2,3,4,5,6,7,8,9,10)

#load data
file_path <- "train.txt"
file_path2 <- "test.txt"
df_train <- read.table(file_path,sep = ",",header = TRUE)
df_test <- read.table(file_path2,sep = ",",header = TRUE)

# #combine train and test
df_train <- df_train[,c(1:12)]
df <- rbind(df_train,df_test)
#delete first column
df[1] <- NULL

#recode categorical data
df_cat <- df[,c(1,2,3,4,5,11)]
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts <- df[,c(6,7,8,9,10)]

df_new <- cbind(df_cat_new, df_cts)
df_new <- scale(df_new)

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  # Give the page a title
  titlePanel("Data clusters"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "Options:", choices=options)
    ),
    mainPanel(
      plotOutput("clustering")
    )
  )
)


server <- function(output,input){
  output$clustering <- renderPlot({
    main_data <- df_new
    k = kmeans(main_data, centers = as.numeric(input$nameInfo), nstart = 25) 
    fviz_cluster(k, geom = "point", data = df_new) + ggtitle("k = ")
  })
  
}

shinyApp(ui,server)
