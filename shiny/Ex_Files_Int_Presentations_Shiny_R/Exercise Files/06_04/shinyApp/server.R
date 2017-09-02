library(WDI)
library(highcharter)
library(forecast)
library(lubridate)

shinyServer(function(input, output) {
  output$xlim_ui <- renderUI({
    if (is.null(input$mean)) {
      return()
    }
    sliderInput(
      "xlim",
      label = "xlim",
      min = input$mean,
      max = 10,
      value = input$mean,
      step = 1
    )
  })
  
  histogram_data <- reactive({
    rnorm(input$no_data, mean = input$mean, sd = input$sd)
  })
  
  output$histogram <- renderHighchart({
    hchart(histogram_data(), name = "Measure") %>%
      hc_title(text = "Interactive Histogram with Highchart") %>%
      hc_tooltip(valuePrefix = "foobar", valueSuffix = 'USD')
  })
  wdi_time_series <- reactive({
    start_year <- year(as.POSIXct(input$date_range[1]))
    end_year <- year(as.POSIXct(input$date_range[2]))
    
    wdi_data <- WDI(
      country = "US",
      indicator = "SP.DYN.TFRT.IN",
      start = start_year,
      end = end_year
    )
    ts(
      data = wdi_data$SP.DYN.TFRT.IN,
      start = start_year,
      end = end_year,
      frequency = 1
    )
  })
  
  output$forecastPlot <- renderHighchart({
    forecast(ets(wdi_time_series()),
             h = input$forecast_n_months / 12,
             level = 95) %>% hchart %>%
      hc_tooltip(
        valuePrefix = "There were ",
        valueSuffix = " births per women",
        valueDecimals = 2
      ) %>%
      hc_title(text = "Fertility Forecast for the US")
  })
  
  output$scatterchart <- renderHighchart({
    highchart() %>% 
      hc_title(text = "Scatter chart with size and color") %>% 
      hc_add_serie_scatter(iris$Sepal.Length, iris$Sepal.Width,
                           iris$Petal.Width, iris$Species)
  })
}
)