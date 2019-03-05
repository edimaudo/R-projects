library(shiny)
library(tidyverse)

sc2018 <- read_csv("sc2018_ip.csv") %>% 
  mutate(Ptype = ifelse(pitch_type %in% c("FC", "FF", "FO", "FS", "FT", "SI"),
                        "fb", "os"))
sc2018 %>% group_by(player_name) %>% summarize(HR = sum(HR)) -> S
plist <- filter(S, HR >= 30) %>% select(player_name) %>% pluck()

ui = fluidPage(
  titlePanel("Locations of 2018 Home Runs: Compare Two 30+ Players"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="player1", label="Choose First Player:", 
                  choices = plist),
      selectInput(inputId="player2", label="Choose Second Player:", 
                  choices = plist),
      radioButtons(inputId="ptype", label="Choose \n Pitch Type",
                   choices = c("All" = "all", "Fastballs" = "fb", "Off-Speed" = "os"))
    ),
    mainPanel(
      plotOutput("myPlot")
    )
  )
)

server = function(input, output) {
  output$myPlot <- renderPlot({
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.95
    outKzone <- 0.95
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    newdata <- filter(sc2018, 
                      player_name %in% c(input$player1, input$player2), HR == 1)
    if(input$ptype == "fb") {newdata <- filter(newdata, Ptype == "fb")} 
    if(input$ptype == "os") {newdata <- filter(newdata, Ptype == "os")}
    ggplot(kZone, aes(x, y)) +
      geom_point(data=newdata,
                 aes(x=plate_x, y=plate_z, color = pitch_type),
                 size = 3) +
      geom_path(lwd=1.5, col="black") +
      coord_fixed() +
      xlim(-1.5, 1.5) + ylim(1, 4) + 
      xlab("") + ylab("") +
      facet_wrap(~ player_name, ncol = 2) + 
      theme(strip.text = element_text(size = 20))
  })
}

shinyApp(ui = ui, server = server)