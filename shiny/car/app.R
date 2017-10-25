#libraries
library(shiny)
library(ggplot2)
library(dplyr)

#load mtcars data
mydata <- mtcars


#Using the standard sidebar layout, create an app that has two dropdown 
#menus in the sidebar and a ggplot2 plot in the main panel. 
#The two dropdown menus select which variables from mtcars will be plotted on the x and y axis. 
#Tip: have a look at aes_string from  ggplot2.

#dd a checkbox which enables the user to choose whether or not to include a stat_smooth.

#Add a checkbox and a dropdown menu that allow the user to toggle the use of facetting and 
#select which variable to facet with.

#plot mpg - x-axis and hp - y-axis

#shiny app
ui <- fluidPage(
  titlePanel("title panel"),
  sidebarLayout(
    sidebarPanel("",
                 selectInput('element_id', label = 'Select X-axis data',
                             choices=colnames(mydata)),
                 selectInput('element_id', label = 'Select Y-axis data',
                             choices=colnames(mydata)),
                 textInput('title_text_box_id', label = 'Enter a title for the plot')
                 ),
    mainPanel("",h1("mtcar data plot"), p("Data plot"),
              plotOutput('dynamicPlot') )
  )
)
server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)
