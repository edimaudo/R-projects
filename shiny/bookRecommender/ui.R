#
# This is a data explorer and Book recommender system
# 

library(shiny)

# Set up the application ui
shinyUI(navbarPage("Book Explorer and Recommendation System",
                   
                   # define the tabs to be used in the app -------
                   # introduction splash
                   tabPanel("Intro",
                            ##  includeMarkdown("./md/intro.md"),
                            hr())))
