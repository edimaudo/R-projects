shinyServer(function(input, output) {
     
     # tabPanel 1 - Draft Board
     
     # Add drafted (deleted) players to a list and add a pick counter
     myValues <- reactiveValues()
     observe({
          myValues$counter <- 1
          if(input$delete.button == 0)
              return()
          isolate({
              myValues$dList <- c(isolate(myValues$dList), input$text)
              myValues$counter <- myValues$counter + 1
          })
          
     })
     
     # Save version of the draft sheet without drafted players
     # Add dynamic probability of pick
     filter <- reactive({
          all.players[!(all.players$PlayerName %in% myValues$dList), -8] %>%
         rowwise() %>%
         mutate(ProbPicked = round(pnorm(myValues$counter, ADP, max(ADP, 36)*(0.42)), 3),
                ProbPickedRound = round(1 - (1 - ProbPicked)^18, 3))
     })
     
     # Display the top 25 entries of the draft sheet
     output$sheet <- DT::renderDataTable({
          if (input$posa != "All" & input$teama == "All") {
               DT::datatable(filter()[grepl(input$posa,filter()$POS) == TRUE,], options = list(pageLength = 25)) %>% formatStyle('Tier', backgroundColor = styleInterval(c(1,2,3,4), c('red', 'orange', 'yellow', 'green', 'blue')), color = styleInterval(c(1,2,3,4), c('white', 'black', 'black', 'white', 'white')))
          }
          else if (input$posa == "All" & input$teama != "All"){
               DT::datatable(filter()[grepl(input$teama,filter()$Team) == TRUE,], options = list(pageLength = 25)) %>% formatStyle('Tier', backgroundColor = styleInterval(c(1,2,3,4), c('red', 'orange', 'yellow', 'green', 'blue')), color = styleInterval(c(1,2,3,4), c('white', 'black', 'black', 'white', 'white')))
          }
          else if (input$posa != "All" & input$teama != "All"){
               DT::datatable(filter()[grepl(input$teama,filter()$Team) == TRUE & grepl(input$posa,filter()$POS) == TRUE,], options = list(pageLength = 25)) %>% formatStyle('Tier', backgroundColor = styleInterval(c(1,2,3,4), c('red', 'orange', 'yellow', 'green', 'blue')), color = styleInterval(c(1,2,3,4), c('white', 'black', 'black', 'white', 'white')))
          }
          else {
               DT::datatable(filter(), options = list(pageLength = 25)) %>% formatStyle('Tier', backgroundColor = styleInterval(c(1,2,3,4), c('red', 'orange', 'yellow', 'green', 'blue')), color = styleInterval(c(1,2,3,4), c('white', 'black', 'black', 'white', 'white')))
          }
          
     })
    
     # tabPanel 2 - Player Tiers
     
     # Save a version of the tier data without drafted players
     tier_current <- reactive({
          tier_dataf <- tier.data[!(tier.data$PlayerName %in% myValues$dList),]
          tierf <- cbind(1:5,as.data.frame(lapply(tier_dataf[,8:16],table))[,seq(2, ncol(tier_dataf[,8:16])*2, by = 2)])
          names(tierf) <- c("Tier", "Pos.C", "Pos.1B", "Pos.2B", "Pos.3B", 
                            "Pos.SS", "Pos.OF", "Pos.SP", "Pos.RP", "Pos.P")
          tierf
     })
     
     # Display the tier distribution for available players
     output$tierc <- DT::renderDataTable({
          DT::datatable(tier_current())
     })
     
     # Display the tier distribution for all players
     output$tiers <- DT::renderDataTable({
          DT::datatable(tier.start)
     })
     
     # tabPanel 3 - Plots

     # Save a version of the tier data as a step in plotting for the selected position
     filterp0 <- reactive({
          # Filter for position
          tier_dataf <- tier.data[grepl(input$pos, tier.data$POS) == TRUE, ]
          
          # Calculate position rank
          tier_dataf <- cbind(1:nrow(tier_dataf), tier_dataf)
          colnames(tier_dataf)[1] <- "Pos.Rank"
          
          #Record whether each player is drafted or not
          tier_dataf$drafted <- ifelse(tier_dataf$PlayerName %in% myValues$dList, 1, 0)
          
          #Return data based on user selection to see drafted players or not
          if(input$drft == "Yes"){
               tier_dataf
          }
          else {
               tier_dataf[tier_dataf$drafted == 0, ]
          }
     })
     
     # Calculate the maximum x-value
     max_x <- reactive({
          if(input$xvar == "Ovr.Rank") {
               max(filterp0()$Ovr.Rank)
          }
          else {
               max(filterp0()$Pos.Rank)
          }
     })
     
     output$ui <- renderUI({
          sliderInput("range", "", min = 1, max = max_x(), value = c(1, 50), width = 800)
     })
     
     # Remove points excluded by the slider
     filterp <- reactive({
          if(input$xvar == "Pos.Rank") {
               filterp0()[filterp0()$Pos.Rank<=max(input$range) & filterp0()$Pos.Rank >= min(input$range), ] 
          }
          else {
               filterp0()[filterp0()$Ovr.Rank<=max(input$range) & filterp0()$Ovr.Rank >= min(input$range), ]
          }
     })
     
     # Function for generating hover tool tip text to show: 
     # name, position, team, and dollar amount
     player_tooltip <- function(x) {
          if (is.null(x)) return(NULL)
          if (is.null(x$Ovr.Rank)) return(NULL)
          
          players <- filterp()
          player <- players[players$Ovr.Rank == x$Ovr.Rank, ]
          
          paste0("<b>", player$PlayerName, "</b><br>",
                 player$POS," ", player$Team, "<br>",
                 player$Dollars
          )
     }
     
     #Create plot
     vis <- reactive({
          
          #Translate x-axis selection
          xvar <- prop("x", as.symbol(input$xvar))
          xvar_name <- names(xaxis)[xaxis == input$xvar]
          
          #Select column for displaying tier by color
          if(input$pos == "P"){
               tier_plot <- as.character(filterp()$Tier.P)
          }
          else {
               tier_plot <- as.character(filterp()[,grep(input$pos, names(filterp()))])
          }
          tvar <- prop("fill", as.symbol("tier_plot"))
          
          filterp() %>%
               ggvis(x = xvar, y = ~Dollars) %>%
               layer_points(size := 50, size.hover := 200,
                            fillOpacity := 0.5, fillOpacity.hover := 0.9, 
                            stroke := "black",key:=~Ovr.Rank,fill=tvar, shape=~factor(drafted))%>%
               add_axis("x", title = xvar_name) %>%
               add_axis("y", title = "Dollars") %>%
               add_legend("fill", title = "Tier", values = c("1", "2", "3", "4", "5")) %>%
               add_legend("shape", title = "Drafted", values = c("Undrafted", "Drafted"),orient="left") %>%
               add_tooltip(player_tooltip, "hover") %>%
               scale_nominal("fill", domain = c("1", "2", "3", "4", "5"),
                             range = c("red", "orange", "yellow", "green", "blue")) %>%
               scale_nominal("shape", domain = c("0", "1"),
                             range = c("circle", "cross")) %>%
               scale_numeric("x", domain = input$range, clamp = TRUE) %>%
               set_options(width = 900, height = 450)
     })
     
     vis %>% bind_shiny("plot1")
     
     #Create plot
     vis2 <- reactive({
       
       #Translate x-axis selection
       xvar <- prop("x", as.symbol(input$xvar))
       xvar_name <- names(xaxis)[xaxis == input$xvar]
       
       #Select column for displaying tier by color
       if(input$pos == "P"){
         tier_plot <- as.character(filterp()$Tier.P)
       }
       else {
         tier_plot <- as.character(filterp()[,grep(input$pos, names(filterp()))])
       }
       tvar <- prop("fill", as.symbol("tier_plot"))
       
       filterp() %>%
         ggvis(x = xvar, y = ~ADP) %>%
         layer_points(size := 50, size.hover := 200,
                      fillOpacity := 0.5, fillOpacity.hover := 0.9, 
                      stroke := "black",key:=~Ovr.Rank,fill=tvar, shape=~factor(drafted))%>%
         add_axis("x", title = xvar_name) %>%
         add_axis("y", title = "ADP") %>%
         add_legend("fill", title = "Tier", values = c("1", "2", "3", "4", "5")) %>%
         add_legend("shape", title = "Drafted", values = c("Undrafted", "Drafted"),orient="left") %>%
         add_tooltip(player_tooltip, "hover") %>%
         scale_nominal("fill", domain = c("1", "2", "3", "4", "5"),
                       range = c("red", "orange", "yellow", "green", "blue")) %>%
         scale_nominal("shape", domain = c("0", "1"),
                       range = c("circle", "cross")) %>%
         scale_numeric("x", domain = input$range, clamp = TRUE) %>%
         set_options(width = 900, height = 450)
     })
     
     vis2 %>% bind_shiny("plot2")
     
})