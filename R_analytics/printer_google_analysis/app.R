#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    tableOutput("data")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$data <- renderTable({
        mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
