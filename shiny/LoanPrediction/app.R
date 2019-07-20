#remove old data
rm(list=ls())
#load libraries
#packages
packages <- c("tidyverse",'shiny','shinydashboard')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

options <- c(2,3,4,5,6,7,8,9,10)

#load data
df_train <- read.table("train.txt",sep = ",",header = TRUE)
df_test <- read.table("text.txt",sep = ",",header = TRUE)

#combine train and test
df_train <- df_train[,c(1:12)]
df <- rbind(df_train,df_test)

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Data clusters"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("nameInfo", "Options:", 
                  choices=options)
    ),
    
    # Create a spot for the barplot
    mainPanel(
      #plotOutput("ageSex"),
      #plotOutput("heightWeight"),
      #plotOutput("medalCount"),
      #plotOutput('gameMedalCount'), 
      #plotOutput("sportMedalCount")
    )
    
  )
)


server <- function(output,input){
  
}

shinyApp(ui,server)
