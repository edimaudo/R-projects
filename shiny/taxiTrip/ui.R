library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

shinyUI(pageWithSidebar(
  headerPanel('NYC Yellow Taxi Trips in January 2015'),
  sidebarPanel(
    selectInput('day', 'Choose the day', choices = c('Sunday' = 1, 'Monday' = 2, 'Tuesday' = 3, 'Wednesday' = 4, 'Thursday' = 5, 'Friday' = 6, 'Saturday' = 7)),
    sliderInput('startHour', 'Choose the starting hour', min = 0, max = 23, value = 3),
    sliderInput('endHour', 'Choose the ending hour', min = 0, max = 23, value = 7)
  ),
  mainPanel(
    p('This shiny app will show you the number of yellow taxi trips that occurred on a particular day between the selected hours.'),
    h4('The day you chose:'),
    verbatimTextOutput('day'),
    h4('The starting hour you chose:'),
    verbatimTextOutput('startHour'),
    h4('The ending hour you chose:'),
    verbatimTextOutput('endHour'),
    h4('The number of yellow taxi trips in the selected time period'),
    plotOutput('taxiPlot')
  )
))