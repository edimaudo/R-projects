library(shiny)
library(data.table)
library(datasets)
library(ggplot2)
library(plotly)
library(dplyr)

merged<-read.csv('clean_merged_data_mlb.csv', header = TRUE, sep = ",")
data<-merged
data$X<-NULL

name_x<-data%>%
  filter(birthYear > 1970)%>%
  select(Name)%>%
  unique()
name<-as.vector(name_x)
sec_name<-as.vector(name_x)
# Define the overall UI
shinyUI(navbarPage("BASEball - Player Comparison",
                   tabPanel("Player Comparison",
                            
                            
                            # Use a fluid Bootstrap layout
                            fluidPage(    
                              
                              # Give the page a title
                              titlePanel("Player Comparison"),
                              
                              
                              fluidRow(
                                column(2),
                                column(4,
                                       h3("Choose First Player:"),
                                       selectInput("sec_Name", "Name:", 
                                                   choices = sec_name, selected = 'Jose Bautista'),
                                       helpText("If multiple player has similar name please choose Birth Year:"),
                                       uiOutput("s_b_year")),
                                
                                #helpText("Choose Age (Choose Age at least 3 years more than youngest Age): "),
                                #uiOutput("ui"),
                                column(6,
                                       h3("Choose Second Player:"),
                                       selectInput("Name", "Name:", 
                                                   choices = name, selected = 'Edwin Encarnacion'),
                                       helpText("If multiple player has similar name please choose Birth Year:"),
                                       uiOutput("b_year"))
                              ),
                              hr(),
                              #helpText("Choose Age (Choose Age at least 3 years more than youngest Age): "),
                              #uiOutput("sec_ui"),
                              fluidRow(
                                column(2),
                                column(4,
                                       helpText("First Player Career Average:"),
                                       tableOutput("f_proj")),
                                
                                column(6, 
                                       helpText("Second Player Career Average:"),
                                       tableOutput("sec_proj"))
                                
                                #plotlyOutput("plotmain")
                              ),
                              fluidRow(
                                column(2),
                                column(4,
                                       helpText("First Player Projected 3 Year Average:"),
                                       tableOutput("f_proj_avg")),
                                
                                column(6, 
                                       helpText("Second Player Projected 3 Year Average:"),
                                       tableOutput("sec_proj_avg"))
                                
                                #plotlyOutput("plotmain")
                              ),
                              hr(),
                              
                              # Create a spot for the plot
                              fluidRow(
                                # plotlyOutput("mlb, Plot") 
                                # column(4,dataTableOutput("values")),
                                # column(3,plotlyOutput("plotmain")),
                                #column(6, plotlyOutput("plotmain",width = "100%", height = "400px")),
                                #column(6, hr()),
                                #column(6, plotlyOutput("plotmain_2",width = "100%", height = "400px")),
                                column(4, plotlyOutput("comparison_HR",width = "100%", height = "400px")),
                                
                                column(4, plotlyOutput("comparison_SLG",width = "100%", height = "400px")),
                                
                                column(4, plotlyOutput("comparison_RBI",width = "100%", height = "400px"))
                                
                              ),
                              hr(),
                              fluidRow(h2("Comparable to Historical Player")),
                              fluidRow(helpText("Utilizes stats from the last three years and calculates historical players that are most similar.")),
                              fluidRow(helpText("Historical players are used to create the Players three year projection.")),
                              fluidRow(
                                column(2),
                                column(4,
                                       helpText("Historical player most similar with First Player"),
                                       tableOutput("sec_com_player")),
                                
                                column(6, 
                                       helpText("Historical player most similar with Second Player"),
                                       tableOutput("f_com_player"))
                              )
                            )
                   )
)



)