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

#load data
file_path <- "Olympics.csv"
olympics <- read.csv(file_path)

country <- unique(olympics$Country)

#filter for one country - #select country
#df_filtered <- df %>%
#  dplyr::filter(Country == "Norway")

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Olympics by country"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("country", "Country:", 
                  choices=colnames(country))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("ageSex")  
    )
    
  )
)



# Define a server for the Shiny app
server <- function(output,input){

    # Fill in the spot we created for a plot
    output$ageSex <- renderPlot({
      ggplot(data=olympics, aes(x=Age, na.rm=TRUE)) + geom_bar(aes(fill = Sex)) + xlab("Age") + 
        ggtitle("Age Vs Sex") + theme_bw()

    })
  
}

shinyApp(ui,server)
