

library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel('Analytics/Data Science Programs in Canada'),
    sidebarPanel(
      sliderInput('endHour', 'Choose the ending hour', min = 0, max = 23, value = 7)
    ),
    mainPanel(
      h4('Options'),
      tabsetPanel(
        tabPanel("Weight", plotOutput("plot")),
        tabPanel("Program Information", tableOutput("table"))
      )
    )
  )
)
