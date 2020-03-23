#Tuesday
#clean up code for different columns that had fix code
#change size of the graphs of all the visualizations
#create headers for third tab


#Wednesday
#Build code for the sections
#final testing


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
#otherText <- read_csv("other_text_responses.csv")
#questions <- read_csv("questions_only.csv")
#surveySchema<- read_csv("survey_schema.csv")
#set.seed(0)

#update multiple choice responses
responses <- multipleChoice %>% 
  slice(2:n()) %>% 
  rename("duration" = `Time from Start to Finish (seconds)`) %>% 
  mutate(Q3 = str_replace_all(Q3, 
        c("United Kingdom of Great Britain and Northern Ireland" = "UK" ,
                                    "United States of America" = "USA",
                                    "Hong Kong \\(S\\.A\\.R\\.\\)" = "Hong Kong",
                                    "Iran, Islamic Republic of..." = "Iran"))) 


#country 
country <- responses %>% 
  group_by(Q3) %>% 
  filter(Q3 != "Other") %>%
  arrange(Q3) %>%
  select(Q3)

country <- as.vector(unique(country$Q3))
  

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
                        highchartOutput("countryplot"),
                        h1("Education",style = "color:#008abc"),
                        h3("Education degrees"),
                        plotOutput("educationplot"),
                        h3("Learning Channels"),
                        plotOutput("educationlearningplot"),
                        h1("Data Science & Machine learning experience",
                           style ="color:#008abc"),
                        h3("Time spent learning to code"),
                        plotOutput("timelearningcodeplot"),
                        h3("Time spend learning machine learning"),
                        plotOutput("timemachinelearningplot"),
                        h1("Employment",style = "color:#008abc"),
                        h3("Pay"),
                        plotOutput("salaryplot"),
                        h3("Time spent"),
                        plotOutput("datasciencetimespent"),
                        h3("Company information"),
                        plotOutput("companysizeplot"),
                        h3("Data Science teams"),
                        plotOutput("datascienceteamsize"),
                        h3("Enterprise machine learning adoption"),
                        plotOutput("mladoption"),
                        h3("Spending"),
                        plotOutput("spending"),
                        h1("Technology",style = "color:#008abc"),
                        h3("IDE usage"),
                        plotOutput("ideusage"),
                        h3("Methods and algorithms usage"),
                        plotOutput("algorithmusage"),
                        h3("Framework usage"),
                        plotOutput("frameworkusage"),
                        h3("Enterprise tools usage"),
                        plotOutput('enterpriseusage'),
                        hr()),
               tabPanel("Country Comparison",
                          sidebarPanel(
                            helpText("Select a country from the two drop down lists"),
                            selectInput("countryInput1", "Country 1",choices=country),
                            selectInput("countryInput2", "Country 2",choices=country),
                            br(),
                            submitButton("Submit")
                          ),
                        mainPanel(
                          plotOutput("coolplot"),
                          br(), br()
                        ),
                        hr())
             ))
)

#gender
genderinfo <- responses %>%
  select(Q2) %>%
  group_by(Q2) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) 

#age
ageinfo <- responses %>%
  select(Q1) %>%
  group_by(Q1) %>%
  summarise(freq = n())

#education degrees
educationinfo <- responses %>%
  select(Q4) %>%
  group_by(Q4) %>%
  summarise(freq = n())
educationinfo <- na.omit(educationinfo)

#time learning to code
timeleanringinfo <- responses %>%
  select(Q15) %>%
  group_by(Q15) %>%
  summarise(freq = n())
timeleanringinfo <- na.omit(timeleanringinfo)

#time machine learning
timeleanringmachineinfo <- responses %>%
  select(Q23) %>%
  group_by(Q23) %>%
  summarise(freq = n())
timeleanringmachineinfo <- na.omit(timeleanringmachineinfo)


# Define server logic
server <- function(input, output) {
  
  #gender
  output$genderplot <- renderPlot({
    ggplot(data=genderinfo, aes(x=reorder(Q2,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Gender", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none") 
  })
  
  #age
  output$ageplot <- renderPlot({
    ggplot(data=ageinfo, aes(x=Q1, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Age", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.position="none")
  })
  
  output$countryplot <- renderHighchart({
    #country
    highchart(type = "map") %>%
      hc_add_series_map(worldgeojson,
                        responses %>% 
                          group_by(Q3) %>% 
                          filter(Q3 != "Other") %>%
                          summarise(total = n()) %>% 
                          ungroup() %>%
                          mutate(iso2 = countrycode(Q3, origin="country.name", destination="iso2c")),
                        value = "total", joinBy = "iso2") %>%
      hc_title(text = "Kagglers by country") %>%
      hc_colorAxis(minColor = "#e8eded", maxColor = "steelblue")
  })
  
  #Education degrees
  output$educationplot <- renderPlot({
    ggplot(data=educationinfo, aes(x=reorder(Q4,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Degree", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") 
    
  })
  
  #education learning -- clean up code
  output$educationlearningplot <- renderPlot({
    
  })
  
  #time learning to code
  output$timelearningcodeplot <- renderPlot({
    ggplot(data=timeleanringinfo, aes(x=reorder(Q15,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time learning to code", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")
  })
  
  #time machine learning
  output$timemachinelearningplot <- renderPlot({
    ggplot(data=timeleanringmachineinfo, aes(x=reorder(Q23,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time spent machine learning", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") 
  })
  
  #salary -- clean up code
  output$salaryplot <- renderPlot({
    salaryinfo <- responses %>%
      select(Q10) %>%
      group_by(Q10) %>%
      summarise(freq = n())
    
    salaryinfo <- na.omit(salaryinfo)
    ggplot(data=salaryinfo, aes(x=Q10, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Salaries", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")
  })
  
  #how data scientist spend their time -- clean up code
  output$datasciencetimespent <- renderPlot({
    
  })
  
  #company size -- clean up code
  output$companysizeplot <- renderPlot({
    companysizeinfo <- responses %>%
      select(Q6) %>%
      group_by(Q6) %>%
      summarise(freq = n())
    
    companysizeinfo <- na.omit(companysizeinfo)
    ggplot(data=companysizeinfo, aes(x=Q6, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "# of Employees", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #data science team size -- fix code
  output$datascienceteamsize <- renderPlot({
    companydatasizeinfo <- responses %>%
      select(Q7) %>%
      group_by(Q7) %>%
      summarise(freq = n())
    
    companydatasizeinfo <- na.omit(companydatasizeinfo)
    ggplot(data=companydatasizeinfo, aes(x=Q7, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + 
      labs(x = "# of Employees in Data Science team", 
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
    
  })
  
  #enterprise machine learning adoption
  output$mladoption <- renderPlot({
    enterpriseinfo  <- responses %>%
      select(Q8) %>%
      group_by(Q8) %>%
      summarise(freq = n())
    enterpriseinfo <- na.omit(enterpriseinfo)
    
    ggplot(data=enterpriseinfo, aes(x=Q8, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + 
      labs(x = "Enterprise machine learning adoption", 
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #spending -- clean up code
  output$spending <- renderPlot({
    enterprisespendinginfo  <- responses %>%
      select(Q11) %>%
      group_by(Q11) %>%
      summarise(freq = n())
    
    enterprisespendinginfo<- na.omit(enterprisespendinginfo)
    ggplot(data=enterprisespendinginfo, aes(x=Q11, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Enterprise spending", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #ide usage
  output$ideusage <- renderPlot({})
  
  #algorithm usage
  output$algorithmusage <- renderPlot({
    
  })
  
  #framework usage
  output$frameworkusage <- renderPlot({})
  
  #enterprise tool usage
  output$enterpriseusage <- renderPlot({})
  
  #plot placeholder
  output$coolplot <- renderPlot({})
}

# Run the application 
shinyApp(ui = ui, server = server)

