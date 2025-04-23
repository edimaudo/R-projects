## 2023 StackOverflow Survey Analysis

## Environment setup
rm(list = ls())

#####Load Libraries###### 
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",
              'scales','ggfortify','DT',
              'shiny','shinydashboard','lubridate','caret',
              'mlbench','mice','countrycode','highcharter',"gridExtra",
              'stopwords','tidytext','stringr','TTR','xts',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#### Load Data ####
df <- read_csv("survey_results_public.csv")

#### UI dropdowns ####
developer_style <- sort(unique(df$MainBranch))
age <- sort(unique(df$Age))
employment <- sort(unique(df$Employment))
work_style <- sort(unique(df$RemoteWork))
education <- sort(unique(df$EdLevel))
devtype <- sort(unique(df$DevType))
orgsize <- sort(unique(df$OrgSize))
country <- sort(unique(df$Country))
industry <- sort(unique(df$Industry))

#### UI ####
ui <- dashboardPage(
  dashboardHeader(title = "2023 Survey Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Experience", tabName = "experience", icon = icon("chart-line")),
      menuItem("Learning", tabName = "learning", icon = icon("graduation-cap")),
      menuItem("Tooling", tabName = "tooling", icon = icon("tools"))
    )
  ),
  dashboardBody(
    tabItems(
      ####### About tab #######
      tabItem(tabName = "about",includeMarkdown("readme.md"),tags$ul()),
      ####### Demographics tab #######
      tabItem(tabName = "demographics",
              h2("Developer Demographics"),
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("demo_age", "Age Group:", 
                                          choices = c("All", age),
                                          selected = "All")),
                    column(4, selectInput("demo_country", "Country:", 
                                          choices = c("All", country),
                                          selected = "All")),
                    column(4, selectInput("demo_employment", "Employment:", 
                                          choices = c("All", employment),
                                          selected = "All"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Age Distribution", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("age_chart")
                ),
                box(
                  title = "Geographic Distribution", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("country_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Education Level", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("education_chart")
                )#,
                ##box(
                ##  title = "Remote Work Patterns", status = "warning", solidHeader = TRUE,
                ##  collapsible = TRUE, width = 6,
                ##  plotOutput("remote_work_chart")
                ##)
              ),
              fluidRow(
                box(
                  title = "Developer Type Breakdown", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("devtype_chart")
                )
              )
      ),
      
      ####### Experience tab #######
      tabItem(tabName = "experience",
              h2("Developer Experience"),
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("exp_devtype", "Developer Type:", 
                                          choices = c("All", devtype),
                                          selected = "All")),
                    column(4, selectInput("exp_orgsize", "Organization Size:", 
                                          choices = c("All", orgsize),
                                          selected = "All")),
                    column(4, selectInput("exp_remote", "Remote Work:", 
                                          choices = c("All", work_style),
                                          selected = "All"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Years Coding Distribution", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("years_code_chart")
                ),
                box(
                  title = "Professional Experience", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("years_pro_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Organization Size Distribution", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("org_size_chart")
                ),
                box(
                  title = "Industry Distribution", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("industry_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Compensation by Experience", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("comp_exp_chart")
                )
              )
      ),
      
      ####### Learning tab #######
      tabItem(tabName = "learning",
              h2("Developer Learning"),
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("learn_age", "Age Group:", 
                                          choices = c("All", age),
                                          selected = "All")),
                    column(4, selectInput("learn_edu", "Education Level:", 
                                          choices = c("All", education),
                                          selected = "All")),
                    column(4, selectInput("learn_exp", "Years of Experience:", 
                                          choices = c("All", "0-3", "4-8", "9-15", "16+"),
                                          selected = "All"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Learning Methods", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("learn_method_chart")
                ),
                box(
                  title = "Online Resources Used", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("online_resources_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Online Courses & Certifications", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("courses_chart")
                ),
                box(
                  title = "Stack Overflow Usage", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("so_usage_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Time Spent Learning Daily", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("time_learning_chart")
                )
              )
      ),
      
      ####### Tooling tab #######
      tabItem(tabName = "tooling",
              h2("Developer Tooling"),
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("tool_devtype", "Developer Type:", 
                                          choices = c("All", devtype),
                                          selected = "All")),
                    column(4, selectInput("tool_exp", "Experience Level:", 
                                          choices = c("All", "Beginner", "Intermediate", "Advanced"),
                                          selected = "All")),
                    column(4, selectInput("tool_country", "Country:", 
                                          choices = c("All", country),
                                          selected = "All"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Programming Languages", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("lang_chart")
                ),
                box(
                  title = "Databases", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("db_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Web Frameworks", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("webframework_chart")
                ),
                box(
                  title = "Cloud Platforms", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("platform_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Development Environments", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("collab_tools_chart")
                )#,
                # box(
                #   title = "Operating Systems", status = "danger", solidHeader = TRUE,
                #   collapsible = TRUE, width = 6,
                #   plotOutput("os_chart")
                # )
              )
              # fluidRow(
              #   box(
              #     title = "AI Tools Usage", status = "danger", solidHeader = TRUE,
              #     collapsible = TRUE, width = 12,
              #     plotOutput("ai_tools_chart")
              #   )
              # )
      )
    )
  )
)

####### Server #######
server <- function(input, output, session) {
  ##### Data #####
  # Filter the data based on inputs
  filtered_demo_data <- reactive({
    data <- df
    
    if (input$demo_age != "All") {
      data <- data %>% filter(Age == input$demo_age)
    }
    if (input$demo_country != "All") {
      data <- data %>% filter(Country == input$demo_country)
    }
    if (input$demo_employment != "All") {
      data <- data %>% filter(str_detect(Employment, input$demo_employment))
    }
    
    return(data)
  })
  
  filtered_exp_data <- reactive({
    data <- df
    
    if (input$exp_devtype != "All") {
      data <- data %>% filter(DevType == input$exp_devtype)
    }
    if (input$exp_orgsize != "All") {
      data <- data %>% filter(OrgSize == input$exp_orgsize)
    }
    if (input$exp_remote != "All") {
      data <- data %>% filter(RemoteWork == input$exp_remote)
    }
    
    return(data)
  })
  
  filtered_learn_data <- reactive({
    data <- df
    
    if (input$learn_age != "All") {
      data <- data %>% filter(Age == input$learn_age)
    }
    if (input$learn_edu != "All") {
      data <- data %>% filter(EdLevel == input$learn_edu)
    }
    if (input$learn_exp != "All") {
      # Convert years of experience to numeric ranges
      years_range <- switch(input$learn_exp,
                            "0-3" = c(0, 3),
                            "4-8" = c(4, 8),
                            "9-15" = c(9, 15),
                            "16+" = c(16, Inf))
      
      # Filter by YearsCode
      data <- data %>%
        filter(!is.na(YearsCode) & YearsCode != "NA") %>%
        filter(as.numeric(YearsCode) >= years_range[1] & as.numeric(YearsCode) <= years_range[2])
    }
    
    return(data)
  })
  
  filtered_tool_data <- reactive({
    data <- df
    
    if (input$tool_devtype != "All") {
      data <- data %>% filter(DevType == input$tool_devtype)
    }
    if (input$tool_exp != "All") {
      # Define experience levels based on YearsCodePro
      exp_levels <- switch(input$tool_exp,
                           "Beginner" = c(0, 3),
                           "Intermediate" = c(4, 8),
                           "Advanced" = c(9, Inf))
      
      data <- data %>%
        filter(!is.na(YearsCodePro) & YearsCodePro != "NA") %>%
        filter(as.numeric(YearsCodePro) >= exp_levels[1] & as.numeric(YearsCodePro) <= exp_levels[2])
    }
    if (input$tool_country != "All") {
      data <- data %>% filter(Country == input$tool_country)
    }
    
    return(data)
  })
  
  ###### Demographics section outputs ######
  output$age_chart <- renderPlot({
    demo_data <- filtered_demo_data()
    
    age_counts <- demo_data %>%
      filter(!is.na(Age)) %>%
      count(Age) %>%
      arrange(desc(n))
    
    ggplot(age_counts, aes(x = reorder(Age, n), y = n)) +
      geom_col(fill = "#F8766D") +
      labs(x = "", y = "", title = "Age Distribution") +
      theme_minimal() +
      coord_flip()
  })
  
  output$country_chart <- renderPlot({
    demo_data <- filtered_demo_data()
    
    country_counts <- demo_data %>%
      filter(!is.na(Country)) %>%
      count(Country) %>%
      arrange(desc(n)) %>%
      slice_max(n, n = 10)
    
    ggplot(country_counts, aes(x = reorder(Country, n), y = n)) +
      geom_col(fill = "#00BA38") +
      labs(x = "", y = "", title = "Top 10 Countries") +
      theme_minimal() +
      coord_flip()
  })
  
  output$education_chart <- renderPlot({
    demo_data <- filtered_demo_data()
    
    # Count by education level
    edu_counts <- demo_data %>%
      filter(!is.na(EdLevel)) %>%
      count(EdLevel) %>%
      arrange(desc(n))
    
    ggplot(edu_counts, aes(x = reorder(EdLevel, n), y = n)) +
      geom_bar(stat = "identity", fill = "#619CFF") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  
  output$remote_work_chart <- renderPlot({
    demo_data <- filtered_demo_data()
    
    # Count by remote work style
    remote_counts <- demo_data %>%
      filter(!is.na(RemoteWork)) %>%
      count(RemoteWork) %>%
      arrange(desc(n))
    
    ggplot(remote_counts, aes(x = "", y = n, fill = RemoteWork)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Remote Work Style") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank())
  })
  
  output$devtype_chart <- renderPlot({
    demo_data <- filtered_demo_data()
    
    # Clean and count DevType (handling multiple responses)
    dev_types <- demo_data %>%
      filter(!is.na(DevType) & DevType != "NA") %>%
      mutate(DevType = strsplit(DevType, ";")) %>%
      unnest(DevType) %>%
      count(DevType) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(dev_types, aes(x = reorder(DevType, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F564E3") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  ###### Experience section outputs #####
  output$years_code_chart <- renderPlot({
    exp_data <- filtered_exp_data()
    
    # Clean and summarize YearsCode
    years_code_data <- exp_data %>%
      filter(!is.na(YearsCode) & YearsCode != "NA" & YearsCode != "Less than 1 year" & YearsCode != "More than 50 years") %>%
      mutate(YearsCode = as.numeric(YearsCode)) %>%
      filter(!is.na(YearsCode))
    
    ggplot(years_code_data, aes(x = YearsCode)) +
      geom_histogram(binwidth = 2, fill = "#00BFC4", color = "white") +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$years_pro_chart <- renderPlot({
    exp_data <- filtered_exp_data()
    
    # Clean and summarize YearsCodePro
    years_pro_data <- exp_data %>%
      filter(!is.na(YearsCodePro) & YearsCodePro != "NA" & YearsCodePro != "Less than 1 year" & YearsCodePro != "More than 50 years") %>%
      mutate(YearsCodePro = as.numeric(YearsCodePro)) %>%
      filter(!is.na(YearsCodePro))
    
    ggplot(years_pro_data, aes(x = YearsCodePro)) +
      geom_histogram(binwidth = 2, fill = "#00BFC4", color = "white") +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$org_size_chart <- renderPlot({
    exp_data <- filtered_exp_data()
    
    # Count by org size
    org_size_counts <- exp_data %>%
      filter(!is.na(OrgSize)) %>%
      count(OrgSize) %>%
      arrange(desc(n))
    
    ggplot(org_size_counts, aes(x = reorder(OrgSize, n), y = n)) +
      geom_bar(stat = "identity", fill = "#B79F00") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$industry_chart <- renderPlot({
    exp_data <- filtered_exp_data()
    
    # Count by industry
    industry_counts <- exp_data %>%
      filter(!is.na(Industry)) %>%
      count(Industry) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(industry_counts, aes(x = reorder(Industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F8766D") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9))
  })
  
  output$comp_exp_chart <- renderPlot({
    exp_data <- filtered_exp_data()
    
    # Clean and prepare compensation data
    comp_data <- exp_data %>%
      filter(!is.na(ConvertedCompYearly) & 
               ConvertedCompYearly > 0 & 
               ConvertedCompYearly < quantile(ConvertedCompYearly, 0.99, na.rm = TRUE) &
               !is.na(YearsCodePro) & 
               YearsCodePro != "NA" &
               YearsCodePro != "Less than 1 year" & 
               YearsCodePro != "More than 50 years") %>%
      mutate(YearsCodePro = as.numeric(YearsCodePro)) %>%
      filter(!is.na(YearsCodePro))
    
    ggplot(comp_data, aes(x = YearsCodePro, y = ConvertedCompYearly)) +
      geom_point(alpha = 0.5, color = "#00BFC4") +
      geom_smooth(method = "loess", se = TRUE, color = "#F564E3") +
      labs(x = "Years of Professional Experience", y = "Annual Compensation (USD)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  ###### Learning section outputs #####
  output$learn_method_chart <- renderPlot({
    learn_data <- filtered_learn_data()
    
    # Parse and count learning methods
    learn_methods <- learn_data %>%
      filter(!is.na(LearnCode)) %>%
      mutate(LearnCode = strsplit(LearnCode, ";")) %>%
      unnest(LearnCode) %>%
      count(LearnCode) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(learn_methods, aes(x = reorder(LearnCode, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00BA38") +
      coord_flip() +
      labs(x = "Learning Method", y = "Count") +
      theme_minimal()
  })
  
  output$online_resources_chart <- renderPlot({
    learn_data <- filtered_learn_data()
    
    # Parse and count online resources
    online_resources <- learn_data %>%
      filter(!is.na(LearnCodeOnline)) %>%
      mutate(LearnCodeOnline = strsplit(LearnCodeOnline, ";")) %>%
      unnest(LearnCodeOnline) %>%
      count(LearnCodeOnline) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(online_resources, aes(x = reorder(LearnCodeOnline, n), y = n)) +
      geom_bar(stat = "identity", fill = "#619CFF") +
      coord_flip() +
      labs(x = "Online Resource", y = "Count") +
      theme_minimal()
  })
  
  output$courses_chart <- renderPlot({
    learn_data <- filtered_learn_data()
    
    # Parse and count online courses
    courses <- learn_data %>%
      filter(!is.na(LearnCodeCoursesCert)) %>%
      mutate(LearnCodeCoursesCert = strsplit(LearnCodeCoursesCert, ";")) %>%
      unnest(LearnCodeCoursesCert) %>%
      count(LearnCodeCoursesCert) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(courses, aes(x = reorder(LearnCodeCoursesCert, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F564E3") +
      coord_flip() +
      labs(x = "Online Course / Certification", y = "Count") +
      theme_minimal()
  })
  
  output$so_usage_chart <- renderPlot({
    learn_data <- filtered_learn_data()
    
    # Count by SO visit frequency
    so_usage <- learn_data %>%
      filter(!is.na(SOVisitFreq)) %>%
      count(SOVisitFreq) %>%
      arrange(desc(n))
    
    ggplot(so_usage, aes(x = reorder(SOVisitFreq, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F8766D") +
      coord_flip() +
      labs(x = "Stack Overflow Visit Frequency", y = "Count") +
      theme_minimal()
  })
  
  output$time_learning_chart <- renderPlot({
    learn_data <- filtered_learn_data()
    
    # Count by time searching and learning
    time_learning <- learn_data %>%
      filter(!is.na(TimeSearching)) %>%
      count(TimeSearching) %>%
      arrange(desc(n))
    
    ggplot(time_learning, aes(x = reorder(TimeSearching, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00BFC4") +
      coord_flip() +
      labs(x = "Daily Time Spent Searching for Solutions", y = "Count") +
      theme_minimal()
  })
  
  ###### Tooling section outputs ######
  output$lang_chart <- renderPlot({
    tool_data <- filtered_tool_data()
    
    # Parse and count languages
    languages <- tool_data %>%
      filter(!is.na(LanguageHaveWorkedWith)) %>%
      mutate(LanguageHaveWorkedWith = strsplit(LanguageHaveWorkedWith, ";")) %>%
      unnest(LanguageHaveWorkedWith) %>%
      count(LanguageHaveWorkedWith) %>%
      arrange(desc(n)) %>%
      head(15)
    
    ggplot(languages, aes(x = reorder(LanguageHaveWorkedWith, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F8766D") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$db_chart <- renderPlot({
    tool_data <- filtered_tool_data()
    
    # Parse and count databases
    databases <- tool_data %>%
      filter(!is.na(DatabaseHaveWorkedWith)) %>%
      mutate(DatabaseHaveWorkedWith = strsplit(DatabaseHaveWorkedWith, ";")) %>%
      unnest(DatabaseHaveWorkedWith) %>%
      count(DatabaseHaveWorkedWith) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(databases, aes(x = reorder(DatabaseHaveWorkedWith, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00BA38") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$webframework_chart <- renderPlot({
    tool_data <- filtered_tool_data()
    
    # Parse and count web frameworks
    frameworks <- tool_data %>%
      filter(!is.na(WebframeHaveWorkedWith)) %>%
      mutate(WebframeHaveWorkedWith = strsplit(WebframeHaveWorkedWith, ";")) %>%
      unnest(WebframeHaveWorkedWith) %>%
      count(WebframeHaveWorkedWith) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(frameworks, aes(x = reorder(WebframeHaveWorkedWith, n), y = n)) +
      geom_bar(stat = "identity", fill = "#619CFF") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
  output$platform_chart <- renderPlot({
    tool_data <- filtered_tool_data()
    
    # Parse and count platforms
    platforms <- tool_data %>%
      filter(!is.na(PlatformHaveWorkedWith)) %>%
      mutate(PlatformHaveWorkedWith = strsplit(PlatformHaveWorkedWith, ";")) %>%
      unnest(PlatformHaveWorkedWith) %>%
      count(PlatformHaveWorkedWith) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(platforms, aes(x = reorder(PlatformHaveWorkedWith, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F564E3") +
      coord_flip() +
      #labs(x = "Cloud Platform", y = "Count") +
      theme_minimal()
  })
  
  output$collab_tools_chart <- renderPlot({
    tool_data <- filtered_tool_data()
    
    # Parse and count collaboration tools
    collab_tools <- tool_data %>%
      filter(!is.na(NEWCollabToolsHaveWorkedWith)) %>%
      mutate(NEWCollabToolsHaveWorkedWith = strsplit(NEWCollabToolsHaveWorkedWith, ";")) %>%
      unnest(NEWCollabToolsHaveWorkedWith) %>%
      count(NEWCollabToolsHaveWorkedWith) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(collab_tools, aes(x = reorder(NEWCollabToolsHaveWorkedWith, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00BFC4") +
      coord_flip() +
      labs(x = "", y = "") +
      theme_minimal()
  })
  
}


####### Run the application #######
shinyApp(ui = ui, server = server)