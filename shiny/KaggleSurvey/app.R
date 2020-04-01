#
# This is a Shiny web application that uses the Kaggle 2019 Data
# Science and Machine learning survey
#

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 
              'caTools','dummies','ggfortify','shiny','countrycode','highcharter',"gridExtra")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
multipleChoice <- read_csv("multiple_choice_responses.csv")

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

country1 <- responses %>% 
  group_by(Q3) %>% 
  filter(Q3 != "Other") %>%
  arrange(desc(Q3)) %>%
  select(Q3)
country1 <- as.vector(unique(country1$Q3))
  

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

#enterprise adoption
enterpriseinfo  <- responses %>%
  select(Q8) %>%
  group_by(Q8) %>%
  summarise(freq = n())
enterpriseinfo <- na.omit(enterpriseinfo)

#enterprise spending
enterprisespendinginfo  <- responses %>%
  select(Q11) %>%
  group_by(Q11) %>%
  summarise(freq = n())
enterprisespendinginfo<- na.omit(enterprisespendinginfo)


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
                        h1("Data Science & Machine learning experience",
                           style ="color:#008abc"),
                        h3("Time spent learning to code"),
                        plotOutput("timelearningcodeplot"),
                        h3("Time spend learning machine learning"),
                        plotOutput("timemachinelearningplot"),
                        h1("Employment",style = "color:#008abc"),
                        h3("Pay"),
                        plotOutput("salaryplot"),
                        h3("Company information"),
                        plotOutput("companysizeplot"),
                        h3("Data Science teams"),
                        plotOutput("datascienceteamsize"),
                        h3("Enterprise machine learning adoption"),
                        plotOutput("mladoption"),
                        h3("Spending"),
                        plotOutput("spending"),
                        hr()),
               tabPanel("Country Comparison",
                        sidebarPanel(
                          helpText("Select countries from the drop down lists"),
                          selectInput("countryInput1", "Country 1",choices=country),
                          selectInput("countryInput2", "Country 2",choices=country1),
                          br(),
                          submitButton("Submit")
                        ),
                        mainPanel(
                          h1("Country Comparisons"),
                          h3("Gender"),
                          plotOutput("gendercountryplot"),
                          h3("Age"),
                          plotOutput("agecountryplot"), 
                          h3("Education Degrees"),
                          plotOutput("educationcountryplot"),
                          h3("Salary"),
                          plotOutput("salarycountryplot"),
                          br(), br()
                        ),
                        hr())
             ))
)


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
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15))
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
    
  })
  
  
  #time learning to code
  output$timelearningcodeplot <- renderPlot({
    ggplot(data=timeleanringinfo, aes(x=factor(Q15,
                                               levels = c("I have never written code",
                                                          "< 1 years","1-2 years",
                                                          "3-5 years","5-10 years","10-20 years",
                                                          "20+ years")), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time learning to code", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
  })
  
  #time machine learning 
  output$timemachinelearningplot <- renderPlot({
    ggplot(data=timeleanringmachineinfo, aes(x=reorder(Q23,-freq), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Time spent machine learning", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
  })
  
  #salary/pay
  output$salaryplot <- renderPlot({

    ggplot(data=salaryinfo, 
           aes(x=factor(Q10, levels = c("$0-999","1,000-1,999","2,000-2,999","3,000-3,999",
                                        "4,000-4,999","5,000-7,499","7,500-9,999","10,000-14,999",
                                        "15,000-19,999","20,000-24,999","25,000-29,999","30,000-39,999",
                                        "40,000-49,999","50,000-59,999","60,000-69,999","70,000-79,999",
                                        "80,000-89,999","90,000-99,999","100,000-124,999", "125,000-149,999", 
                                        "150,000-199,999","200,000-249,999",
                                        "250,000-299,999","300,000-500,000",
                                        "> $500,000")),
               y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Salaries", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15))
  })
  
  
  #company size
  output$companysizeplot <- renderPlot({
    ggplot(data=companysizeinfo, aes(x=factor(Q6,levels=c("0-49 employees","50-249 employees",
                                                          "250-999 employees","1000-9,999 employees",
                                                          "> 10,000 employees")), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "# of Employees", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
  })
  
  #data science team size
  output$datascienceteamsize <- renderPlot({
    ggplot(data=companydatasizeinfo, aes(x=factor(Q7,levels = c("0","1-2","3-4","5-9","10-14",
                                                                "15-19","20+")), y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + 
      labs(x = "Data Science team size", 
           y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
    
  })
  
  #enterprise machine learning adoption
  output$mladoption <- renderPlot({
    ggplot(data=enterpriseinfo, aes(x=Q8, y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + 
      labs(x = "machine learning adoption", 
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 10)) + coord_flip()
  })
  
  #spending
  output$spending <- renderPlot({
    
    ggplot(data=enterprisespendinginfo, aes(x=factor(Q11,levels=c("$0 (USD)","$1-$99",
                                                                   "$100-$999","$1000-$9,999",
                                                                   "$10,000-$99,999","> $100,000 ($USD)")), 
                                            y=freq)) +
      geom_bar(stat="identity",fill="steelblue") + theme_classic() + labs(x = "Enterprise spending", 
                                                                          y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position="none",axis.title = element_text(size = 25),
            axis.text = element_text(size = 15)) + coord_flip()
  })
  
  #plot placeholder
  output$gendercountryplot <- renderPlot({
    gendercountryinfo <- responses %>%
      select(Q2,Q3) %>%
      filter(Q2 %in% c('Male','Female')) %>%
      filter(Q3 %in% c(input$countryInput1,input$countryInput2)) %>%
      group_by(Q2,Q3) %>%
      summarize(freq = n()) %>%
      arrange(desc(freq)) 
    
    ggplot(data=gendercountryinfo, aes(x=reorder(Q2,-freq), y=freq, fill=Q3)) +
      geom_bar(stat="identity",position=position_dodge(),width = 0.8) + theme_classic() + 
      labs(x = "Gender", y = "Count") +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15)) + 
      labs(fill = "Country")
    
    
  })
  output$agecountryplot <- renderPlot({
    agecountryinfo <- responses %>%
      select(Q1,Q3) %>%
      filter(Q3 %in% c(input$countryInput1,input$countryInput2)) %>%
      group_by(Q1,Q3) %>%
      summarise(freq = n())
    
    ggplot(data=agecountryinfo, aes(x=reorder(Q1,-freq), y=freq, fill=Q3)) +
      geom_bar(stat="identity",position=position_dodge(),width = 0.8) + theme_classic() + 
      labs(x = "Age", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 15)) + 
      labs(fill = "Country")
  })
  output$educationcountryplot <- renderPlot({
    educationcountryinfo <- responses %>%
      select(Q4,Q3) %>%
      filter(Q3 %in% c(input$countryInput1,input$countryInput2)) %>%
      group_by(Q4,Q3) %>%
      summarise(freq = n())
    educationcountryinfo <- na.omit(educationcountryinfo)
    
    ggplot(data=educationcountryinfo, aes(x=reorder(Q4,-freq), y=freq, fill=Q3)) +
      geom_bar(stat="identity",position=position_dodge(),width = 0.8) + theme_classic() + 
      labs(x = "Degree", y = "Count")  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 10)) + 
      labs(fill = "Country") + scale_y_continuous(breaks = c(20,40,60,80,100))
    
  })
  output$salarycountryplot <- renderPlot({
    salarycountryinfo <- responses %>%
      select(Q10,Q3) %>%
      filter(Q3 %in% c(input$countryInput1,input$countryInput2)) %>%
      group_by(Q10,Q3) %>%
      summarise(freq = n())
    salarycountryinfo <- na.omit(salarycountryinfo)
    
    ggplot(data=salarycountryinfo, 
           aes(x=factor(Q10, levels = c("$0-999","1,000-1,999","2,000-2,999","3,000-3,999",
                                        "4,000-4,999","5,000-7,499","7,500-9,999","10,000-14,999",
                                        "15,000-19,999","20,000-24,999","25,000-29,999","30,000-39,999",
                                        "40,000-49,999","50,000-59,999","60,000-69,999","70,000-79,999",
                                        "80,000-89,999","90,000-99,999","100,000-124,999", "125,000-149,999", 
                                        "150,000-199,999","200,000-249,999",
                                        "250,000-299,999","300,000-500,000",
                                        "> $500,000")),
               y=freq,fill=Q3)) +
      geom_bar(stat="identity",position=position_dodge(),width = 0.8) + theme_classic() + 
      labs(x = "Salary", y = "Count")  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 20),axis.text = element_text(size = 10)) + 
      labs(fill = "Country") 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

