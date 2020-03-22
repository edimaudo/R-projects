#
# This is a Shiny web application that uses the Kaggle 2019 Data
# Science and Machine learning survey
#

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','shiny','countrycode','highcharter')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
multipleChoice <- read_csv("multiple_choice_responses.csv")
otherText <- read_csv("other_text_responses.csv")
questions <- read_csv("questions_only.csv")
surveySchema<- read_csv("survey_schema.csv")

set.seed(0)

#update multiple choice responses
responses <- multipleChoice %>% 
  slice(2:n()) %>% 
  rename("duration" = `Time from Start to Finish (seconds)`) %>% 
  mutate(Q3 = str_replace_all(Q3, 
        c("United Kingdom of Great Britain and Northern Ireland" = "UK" ,
                                    "United States of America" = "USA",
                                    "Hong Kong \\(S\\.A\\.R\\.\\)" = "Hong Kong",
                                    "Iran, Islamic Republic of..." = "Iran"))) 




#Monday
#add code for second tab
#clean up code for different columns that had fix code
#country comparison
#add side bar panel
#add two drop downs with county information sorted in ascending order
#add a submit button

#Tuesday
#create headers
#create sections for the differnet countries
#Build code for the sections

#Wednesday
#add code for the section
#finalize build + complete

ui <- fluidPage(
  navbarPage("2019 Kaggle and Machine Learning Survey",
             tabsetPanel(
               tabPanel("Introduction",
                        includeMarkdown("intro.md"),
                        hr()),
               tabPanel("Survey Key Insights",
                        h1("Data Scientist Profile",style = "color:#008abc"),
                        h3("Gender"),
                        plotOutput("genderplot"),
                        h3("Age"),
                        plotOutput("ageplot"),
                        h3("Country"),
                        h1("Education",style = "color:#008abc"),
                        h3("Education degrees"),
                        h3("Learning Channels"),
                        h1("Data Science & Machine learning experience",
                           style ="color:#008abc"),
                        h3("Time spent learning to code"),
                        h3("Time spend learning machine learning"),
                        h1("Employment",style = "color:#008abc"),
                        h3("Pay"),
                        h3("Time spent"),
                        h3("Company information"),
                        h3("Data Science teams"),
                        h3("Enterprise machine learning adoption"),
                        h3("Spending"),
                        h1("Technology",style = "color:#008abc"),
                        h3("IDE usage"),
                        h3("Methods and algorithms usage"),
                        h3("Framework usage"),
                        h3("Enterprise tools usage"),
                        hr()),
               tabPanel("Country Comparison",
                        hr())
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #gender
  output$genderplot <- renderPlot({
    genderinfo <- responses %>%
      select(Q2) %>%
      group_by(Q2) %>%
      summarize(freq = n()) %>%
      arrange(desc(freq)) 
    
    ggplot(data=genderinfo, aes(x=reorder(Q2,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Gender", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none") 
  })
  
  #age
  output$ageplot <- renderPlot({
    ageinfo <- responses %>%
      select(Q1) %>%
      group_by(Q1) %>%
      summarise(freq = n())
    
    ggplot(data=ageinfo, aes(x=Q1, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Age", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.position="none")
  })
  
  output$ageplot <- renderPlot({})

}

# Run the application 
shinyApp(ui = ui, server = server)

