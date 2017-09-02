library(shiny)
shinyApp(
  ui = navbarPage(
    "Example navbarPage",
    tabPanel("Histograms",
    fluidPage(sidebarLayout(
      sidebarPanel(
        sliderInput("no_data", label = "Number of data",
                    min = 1000,
                    max = 5000,
                    value = 1000),
        sliderInput("mean", label = "Mean",
                    min = 0,
                    max = 8,
                    value = 3),
        sliderInput("sd", label = "Standard Deviation",
                    min = 1,
                    max = 10,
                    value = 2),
        uiOutput("xlim_ui")
      ),
      mainPanel(plotOutput("histogram"))
    ))
    ),
    navbarMenu(
      "What's Next?",
      tabPanel("Shiny Themes",
               "insert shiny themes thing here"),
      tabPanel("Interactive Charts",
               "insert interactive chart here")
    ), collapsible = TRUE
  ),
  server = function(input, output){
    
    output$xlim_ui <- renderUI({
      if(is.null(input$mean)){
        return()
      }
      sliderInput("xlim", label = "xlim",
                  min = input$mean,
                  max = 10,
                  value = input$mean,
                  step = 1)
    })
    
    data_for_plot <- reactive({
      rnorm(input$no_data, mean = input$mean, sd = input$sd)
    })
    
    output$histogram <- renderPlot({
      if(is.null(input$xlim)){
        return()
      }
      
      data_for_plot <- data_for_plot()
      
      hist(data_for_plot, xlim = c(-input$xlim,input$xlim))
    })
  }
)


## Histograms - bin width

