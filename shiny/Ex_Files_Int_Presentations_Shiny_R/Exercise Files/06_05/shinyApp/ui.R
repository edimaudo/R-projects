llibrary(highcharter)
shinyUI(navbarPage(
  "highcharter dashboars",
  tabPanel("Fertility Forecast",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   "date_range",
                   label = "Date Range",
                   min = "1960-01-01",
                   max = "2012-12-01",
                   start = "1960-06-06",
                   end = "2012-01-01",
                   format = "yyyy-mm-dd",
                   startview = "decade"
                 ),
                 sliderInput(
                   "forecast_n_months",
                   min = 12,
                   max = 12 * 5,
                   value = 48,
                   step = 12,
                   label = "Forecast n months"
                 )
               ),
               mainPanel(highchartOutput("forecastPlot"))
             )
           )),
  navbarMenu(
    "More Examples",
    tabPanel("Interactive Histogram",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     "no_data",
                     label = "Number of data",
                     min = 1000,
                     max = 5000,
                     value = 1000
                   ),
                   sliderInput(
                     "mean",
                     label = "Mean",
                     min = 0,
                     max = 8,
                     value = 3
                   ),
                   sliderInput(
                     "sd",
                     label = "Standard Deviation",
                     min = 1,
                     max = 10,
                     value = 2
                   ),
                   uiOutput("xlim_ui")
                 ),
                 mainPanel(highchartOutput("histogram"))
               )
             )),
    tabPanel("Interactive Scatterchart",
             fluidPage(
               highchartOutput("scatterchart")
             ))
  ),
  collapsible = TRUE
))