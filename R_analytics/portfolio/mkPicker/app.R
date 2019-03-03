library(shiny)
library(shinydashboard)

library(tidyverse)
library(DT)

# load("C:/Users/Derek/Google Drive/bootcamp/Project2/mk/mkPicker/mkItem.rdata")
load("mkItem.rdata")

ui <- dashboardPage(
    # dashboard header
    dashboardHeader(title = "mkPicker"),

    # dashboard sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            menuItem("Picker", tabName = "picker", icon = icon("th")),
            menuItem("Recommender (WIP)", tabName = "recommender", icon = icon("th")),
            menuItem("About", tabName = "about", icon = icon("user"))
        )
    ),

    # dashboard body
    dashboardBody(
        # includeCSS("C:/Users/Derek/Google Drive/bootcamp/Project2/mk/mkPicker/www/custom.css"),
        tags$head(includeCSS("www/custom.css")),
        tabItems(
            # intro
            tabItem(tabName = "intro",
                    fluidRow(
                        column(width = 1),
                        column(width = 5,
                               h2("Pre-built Mechanical Keyboard Picker/Recommender"),
                               a(href = "https://github.com/siyuanligit/mk", "Github link for this project"),
                               h3("Introduction:"),
                               p("Getting into the world of Mechanical Keyboard can be daunting, so many choices, so many brands, switch types, lightings, etc. This app is designed to help you pick a mechanical keyboard of your preference."),
                               h3("Data:"),
                               p('Data is scraped from mechanicalkeyboards.com, using Scrapy in Python.'),
                               p("Product specifications are spanned using different switches available and their corresponding prices."),
                               h3("How to use this webApp:"),
                               p("First part is a simple filtering picker. Provide you with a list of choices based on your answer to each question."),
                               p("Second part is a recommendation system which will give you a top 5 pick of mechanical keyboards based on your choice."),
                               h3("Future Works:"),
                               p("Some NLP analysis using the product reviews. ")),
                        column(width = 5))),

            # picker/filter
            tabItem(tabName = "picker",
                    column(width = 4,
                           box(width = NULL,
                               selectInput("use", "What is your primary use of your keyboard?",
                                           choices = c("Not Sure", "Gaming", "Typing", "Both")),
                               selectInput("size", "Do you need the number pad?",
                                           choices = c("Not Sure", "Yes", "No", "I like my keyboard small")),
                               selectInput("noise", "What is your tolerable level of noise, from your keyboard?",
                                           choices = c("Not Sure","I like them LOUD", "Don't be too loud", "Shhhhhh!")),
                               selectInput("kbcolor", "What color theme do you like on your keyboard?",
                                           choices = c("Not Sure", "White on Black", "White", "Old School", "PINK")),
                               selectInput("backlit", "Would you like backlighting on your keyboard?",
                                           choices = c("Not Sure", "Nah", "Sure", "RGB!!!!!!")),
                               hr(),
                               actionButton("submit", "Submit")
                           )),
                    column(width = 8,
                           fluidRow(box(width = 4, height = NULL, htmlOutput("productImage", height = 200)),
                                    box(width = 4, height = NULL, htmlOutput("productSummary")),
                                    htmlOutput("rgbfun", click = "rgbfun_click")),
                           fluidRow(box(width = NULL, height = NULL, class = "dataTable",
                                        dataTableOutput("pickerTable"))))),

            # recommender
            tabItem(tabName = "recommender"),

            # about
            tabItem(tabName = "about",
                    column(width = 1),
                    column(width = 11,
                           h2("About Author:"),
                           img(src = "face.jpg"),
                           h3("Siyuan Li, Derek"),
                           h4("Data Science Fellow at NYC Data Science Academy"),
                           h4("Volunteer at DataKind.org"),
                           br(),
                           h4("Master of Applied Statisitcs, UCLA 2016 - 2018"),
                           h4("Bachelor of Financial Mathematics and Statistics, UCSB 2011 - 2015"),
                           br(),
                           h4("Rental Real Estate Broker Assistant, Underwriting 2018"),
                           h4("Quantitative Analyst Intern, Mingshi Investment Management, Shanghai, China 2016"),
                           h4("Investment Analyst Intern, Soochow Securities, Suzhou, Jiangsu, China 2015"),
                           br(),
                           a(href="www.linkedin.com/in/siyuan-derek-li-b4663a49", "LinkedIn"),
                           br(),
                           a(href="github.com/siyuanligit", "Github")))
        )
    )
)

# server functionality
server <- function(input, output) {
    
    observeEvent(input$submit, {
        print(input$use)
        print(input$size)
        print(input$noise)
        print(input$kbcolor)
        print(input$backlit)
        print(input$pickerTable_rows_selected)
    })
    
    observeEvent(input$pickerTable_rows_selected, {
        print(input$pickerTable_rows_selected)
    })
    
    ans = eventReactive(input$submit, {

        if (input$use == "Not Sure" | input$use == "Both") {
            q1 = mkItems2
        } else if (input$use == "Gaming") {
            q1 = mkItems2 %>%
                filter(grepl('(Black|Red|Brown|Silver|Blue|Linear|Click)', switch))
        } else if (input$use == "Typing") {
            q1 = mkItems2 %>%
                filter(grepl('(Black|Brown|Blue|Green|White|Linear|Click|Topre)', switch))
        }
    
        if (input$size == "Not Sure") {
            q2 = q1
        } else if (input$size == "Yes") {
            q2 = q1 %>% 
                filter(size == "Full Size")
        } else if (input$size == "No") {
            q2 = q1 %>% 
                filter(size %in% c("Tenkeyless", "60%", "Other"))
        } else if (input$size == "I like my keyboard small") {
            q2 = q1 %>% 
                filter(size %in% c("60%", "Other"))
        }

        if (input$noise == "Not Sure") {
            q3 = q2
        } else if (input$noise == "I like them LOUD") {
            q3 = q2 %>% 
                filter(grepl('(Blue|Green|White|Matias Click)', switch))
        } else if (input$noise == "Don't be too loud") {
            q3 = q2 %>% 
                filter(grepl('(Black|Red|Brown|Silver|Clear|Topre|Linear)', switch))
        } else if (input$noise == "Shhhhhh!") {
            q3 = q2 %>% 
                filter(grepl('(Quiet Click|Silent)', switch))
        }
        
        if (input$kbcolor == "Not Sure") {
            q4 = q3
        } else if (input$kbcolor == "White on Black") {
            q4 = q3 %>% 
                filter(frcolor == "Black", color == "Black", legend == "White")
        } else if (input$kbcolor == "White") {
            q4 = q3 %>% 
                filter(frcolor %in% c("White", "Silver"), color == "White")
        } else if (input$kbcolor == "Old School") {
            q4 = q3 %>% 
                filter(frcolor %in% c("White", "Gray"), color == "White", legend == "Black")
        } else if (input$kbcolor == "PINK") {
            q4 = q3 %>% 
                filter(frcolor == "Pink")
        }
        
        if (input$backlit == "Not Sure") {
            q5 = q4
        } else if (input$backlit == "Nah") {
            q5 = q4 %>% 
                filter(led == "n/a")
        } else if (input$backlit == "Sure") {
            q5 = q4 %>% 
                filter(!led %in% c("n/a", "RGB"))
        } else if (input$backlit == "RGB!!!!!!") {
            q5 = q4 %>% 
                filter(led == "RGB", grepl('(RGB|rgb)', name))
        }
        
        q5 %>%
            select(name, switch, price, averating, img, led, nreviews) %>% 
            filter(averating != "") %>% 
            arrange(desc(nreviews))
    })
    
    picLink = reactive({
        if (nrow(ans()) != 0) {
            if (!is.null(input$pickerTable_rows_selected)) {
                ans() %>% 
                    filter(row_number() == input$pickerTable_rows_selected)
            } else {
                ans() %>% 
                    filter(row_number() == 1)
            }
        }
    })
    
    output$productImage = renderText({
        c('<img src="',picLink() %>% select(img) %>% pull(),'">')
    })
    
    output$productSummary = renderText({
        if (nrow(picLink()) == 0) {
            paste0('<h3>Please widen your search!</h3>')
        } else {
            paste0('<h4>Your pick:</h4>',
                   '<h4>', picLink() %>% select(name) %>% pull(), '</h4>',
                   '<p>- Switch: ', picLink() %>% select(switch) %>% pull(), "</p>",
                   '<p>- Price: $', picLink() %>% select(price) %>% pull(), "</p>",
                   '<p>- LEDs: ', picLink() %>% select(led) %>% pull(), "</p>")
        }
    })

    output$pickerTable = renderDataTable({
        if (nrow(ans()) != 0) {
            ans() %>% select(-img, -led, -nreviews)
        }
    }, options = list(pageLength = 10), selection = 'single')
    
    output$rgbfun = renderText({
        if (nrow(ans()) != 0) {
            if (picLink() %>% select(led) %>% pull() == "RGB") {
                c('<img class="rgbfun" src="rgb.gif">')
            }
        }
    })
    
    observeEvent(input$rgbfun_click,{
        removeUI(selector = "img.rgbfun")
    })
}

shinyApp(ui = ui, server = server)

