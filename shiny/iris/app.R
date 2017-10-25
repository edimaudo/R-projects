library(shiny)

shinyUI(fluidPage(
  
  #fluid page for dynamically adapting to screens of different resolutions.
  titlePanel("Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      #implementing radio buttons
      radioButtons("p", "Select column of iris dataset:",
                   list("Sepal.Length"='a', "Sepal.Width"='b', "Petal.Length"='c', "Petal.Width"='d')),
      
      #slider input for bins of histogram
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
      # Show a plot of the generated distribution
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

shinyServer(function(input, output) {
  
  #referring output distPlot in ui.r as output$distPlot
  output$distPlot <- renderPlot({
    
    #referring input p in ui.r as input$p
    if(input$p=='a'){
      i<-1
    }
    
    if(input$p=='b'){
      i<-2
    }
    
    if(input$p=='c'){
      i<-3
    }
    
    if(input$p=='d'){
      i<-4
    }
    
    x    <- iris[, i]
    
    #referring input bins in ui.r as input$bins
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #producing histogram as output
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
})

shinyApp(ui = ui, server = server)