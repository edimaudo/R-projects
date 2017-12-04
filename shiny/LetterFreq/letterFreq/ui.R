#ui.R

shinyUI(fluidPage(
  theme="bootstrap.css",
  titlePanel("LetterFreq!"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        helpText(HTML("<b>ABOUT</b>")),
        helpText(HTML("Plot the distribution of English letter frequencies
                      of the input text, and optionally show a reference distribution.
                      <br><br>
                      
                      Input can include any characters, but is truncated to 30,000 chars total. Text is 
                      sanitized to keep only the English letters.
                      Number of occurences for each letter is divided by the total letter count to obtain
                      relative frequencies. Results are shown as a bar plot, with absolute counts next to each
                      bar.
                      <br><br>
                      
                      To use, enter text in the textbox below, choose a reference, and hit Plot."))
        ),
      
      wellPanel(
        textInput("text1",HTML("<b>ENTER SOME TEXT</b>"),"LetterFreq!"),
        radioButtons("ref", 
                     label = HTML("<b>REFERENCE DISTRIBUTION</b>"),
                     choices = list("English literature classics","Oxford dictionary","None"),
                     selected = "None"),
        
        submitButton("Plot")
        
      ),
      
      wellPanel(
        helpText(HTML("<b>REFERENCE DATA SOURCES</b>")),
        helpText(HTML("<a href='http://www.data-compression.com/english.html'>English literature classics</a>")),
        helpText(HTML("<a href='http://www.oxforddictionaries.com/us/words/what-is-the-frequency-of-the-letters-of-the-alphabet-in-english'>Oxford dictionary</a>"))
        
      ),
      
      wellPanel(
        helpText(HTML("<a href='https://github.com/edimaudo'>Github source</a>"))
      )
        ),
    
    mainPanel(plotOutput("freqplot"))        
        )
    ))