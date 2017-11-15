
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Shiny App"),
  sidebarLayout(
    sidebarPanel( h2("Menu"),
                  img(src="petal.jpg", height = 72, width = 100)) ,
    mainPanel(h1("Main"),br() ,h2("Analysis"),
              p("This famous (Fisher’s or Anderson’s)", a("iris","http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html"), 
                "data set gives the measurements
                in centimeters of the variables sepal length and width and petal length and width, 
                respectively, for 50 flowers from each of 3 species of iris. 
                The species are Iris",strong("setosa, "),strong("versicolor, "),"and",strong("virginica.")) 
              ) )))
