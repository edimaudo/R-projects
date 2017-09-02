shinyServer(function(input, output){
  output$curvePlot <- renderPlot(
    curve(x^input$exponent, from = -5, to = 5)
  )
})
