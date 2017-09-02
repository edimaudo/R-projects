library(shiny)
shinyApp(
  ui = fluidPage(
    sliderInput(
      "exponent",
      label = "Exponent",
      min = 1,
      max = 5,
      value = 2
    ),
    plotOutput("curvePlot")
  ),
  server = function(input, output) {
    output$curvePlot <-
      renderPlot({
        curve(x ^ input$exponent, from = -5, to = 5)
      })
  }
)