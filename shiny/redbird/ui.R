shinyUI(navbarPage("Fantasy Baseball Draft App",
     tabPanel("Draft Board",
          fluidPage(
               fluidRow(align="left",
                    # Selection list for position
                    column(4, selectInput('posa', 'Position', positions.all)),
                    # Selection list for position
                    column(4, selectInput('teama', 'Team', teams.all)),
                    # Text input to remove players when they are drafted
                    column(4, strong('Player'), br(),
                           textInput.typeahead(id = "text", 
                                               placeholder = "", 
                                               local = autocomplete.list,
                                               valueKey = "PlayerName",
                                               tokens = c(1:dim(autocomplete.list)[1]),
                                               template="<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{PlayerName}}</p>")),
                    
                    # Delete button 
                    column(4, br(), actionButton(inputId = "delete.button", label = "Delete", icon = icon("minus")))
               ),
               fluidRow(align = "center",
                    # Draft sheet
                    DT::dataTableOutput("sheet")
               )
          )
     ),
     tabPanel("Player Tiers",
              fluidPage(
                   fluidRow(align = "center",
                        # Display tiers after removing drafted players
                        h4("Current Tiers"),
                        DT::dataTableOutput("tierc")
                   ),
                   fluidRow(align = "center",
                        # Display tiers at the start of the draft
                        h4("Starting Tiers"),
                        DT::dataTableOutput("tiers")
                   )
              )
     ),
     tabPanel("Plots",
              fluidPage(
                   fluidRow(align = "left",
                        # Selection list for position
                        column(4, selectInput('pos', 'Position', positions)),
                        # Choose plotting by overall rank or position rank
                        column(4, selectInput('xvar', 'Rank', xaxis)),
                        column(4, selectInput('drft', 'Show Drafted', c("Yes", "No")))
                   ),
                   fluidRow(align = "center",
                        h4("Talent by Position Rank"),
                        ggvisOutput("plot1")
                   ),
                   fluidRow(align = "center",
                        uiOutput("ui")
                   ),
                   fluidRow(align = "center",
                            h4("ADP by Position Rank"),
                            ggvisOutput("plot2")
                   )
              )
     ),
     tabPanel("Help",
              fluidPage(
                   includeMarkdown("README.md")     
              )
     )
     ))