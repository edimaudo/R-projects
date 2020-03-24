#Tuesday
#clean up code for areas that have different factors
#get code for merging different columns
#move data manuipulation out of server function
#change font sizes

#Wednesday
#create headers for third tab
#Build code for the sections
#final testing


#
# This is a Shiny web application that uses the Kaggle 2019 Data
# Science and Machine learning survey
#

#rm(list=ls())
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
                            helpText("Select countries from the drop down lists"),
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

#salary
salaryinfo <- responses %>%
  select(Q10) %>%
  group_by(Q10) %>%
  summarise(freq = n())
salaryinfo <- na.omit(salaryinfo)

#company size
companysizeinfo <- responses %>%
  select(Q6) %>%
  group_by(Q6) %>%
  summarise(freq = n())
companysizeinfo <- na.omit(companysizeinfo)

#data science team size
companydatasizeinfo <- responses %>%
  select(Q7) %>%
  group_by(Q7) %>%
  summarise(freq = n())
companydatasizeinfo <- na.omit(companydatasizeinfo)

# Define server logic
server <- function(input, output) {
  
  #gender
  output$genderplot <- renderPlot({
    ggplot(data=genderinfo, aes(x=reorder(Q2,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Gender", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
  })
  
  #age
  output$ageplot <- renderPlot({
    ggplot(data=ageinfo, aes(x=Q1, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Age", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.position="none",
            axis.title = element_text(size = 50))
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
    
  })
  
  #education learning -- clean up code
  output$educationlearningplot <- renderPlot({
    
  })
  
  #time learning to code
  output$timelearningcodeplot <- renderPlot({
    ggplot(data=timeleanringinfo, aes(x=factor(Q15,
                                               levels = c("I have never written code","< 1 years","1-2 years",
                                                          "3-5 years","5-10 years","10-20 years",
                                                          "20+ years")), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time learning to code", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #time machine learning 
  output$timemachinelearningplot <- renderPlot({
    ggplot(data=timeleanringmachineinfo, aes(x=reorder(Q23,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time spent machine learning", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #salary
  output$salaryplot <- renderPlot({

    ggplot(data=salaryinfo, 
           aes(x=factor(Q10, levels = c("0-999","1,000-1,999","2,000-2,999","3,000-3,999",
                                        "4,000-4,999","5,000-7,499","7,500-9,999","10,000-14,999",
                                        "15,000-19,999","20,000-24,999","25,000-29,999","30,000-39,999",
                                        "40,000-49,999","50,000-59,999","60,000-69,999","70,000-79,999",
                                        "80,000-89,999","90,000-99,999","100,000-124,999", "125,000-149,999", 
                                        "150,000-199,999","200,000-249,999","250,000-299,999","300,000-500,000",
                                        "> 500,000")),
               y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Salaries", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")
  })
  
  #how data scientist spend their time -- clean up code
  output$datasciencetimespent <- renderPlot({
    
  })
  
  #company size
  output$companysizeplot <- renderPlot({

    ggplot(data=companysizeinfo, aes(x=factor(Q6,levels=c("0-49 employees","50-249 employees",
                                                          "250-999 employees","1000-9,999 employees",
                                                          "> 10,000 employees")), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "# of Employees", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") + coord_flip()
  })
  
  #data science team size
  output$datascienceteamsize <- renderPlot({
    ggplot(data=companydatasizeinfo, aes(x=factor(Q7,levels = c("0","1-2","3-4","5-9","10-14",
                                                                "15-19","20+")), y=freq)) +
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

