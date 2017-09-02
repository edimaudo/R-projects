shinyUI(
  fluidPage(
  sliderInput("exponent",
              label = "Exponent",
              min = 1,
              max = 5,
              value = 2),
  plotOutput("curvePlot")
))