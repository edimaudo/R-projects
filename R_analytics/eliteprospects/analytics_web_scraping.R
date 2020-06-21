####################################################################
# Objective - scrap player data from https://www.eliteprospects.com
# Get Player facts
# Get Default player statistics
# Get Default tournament statistics
####################################################################
#clear environment
rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','rvest','stringr',
              'lubridate','rebus','magrittr')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#set working directory to where your data is located

#load data
df <- read.csv("players_new2.csv", sep=";")

#remove duplicates
df <- df %>% distinct()

#select playerid and player url
df2 <- df %>%
  select(playerID,player_url) #%>%
  
#create player facts data frame
player_facts_all <- data.frame(matrix(ncol = 8, nrow = 0))
#column information for player facts
player_fact_col <- c("player_id","date_of_birth",
                     "place_of_birth","nation",
                     "youth_team","position",'shoots','status')
colnames(player_facts_all) <- player_fact_col

#create player statistics dataframe
player_statistics_all <- data.frame(matrix(ncol = 18, nrow = 0))
player_statistics_all2 <- data.frame(matrix(ncol = 24, nrow = 0))
#column information for player statistics 
player_statistics_col <- c("S","Team","League","GP","G","A","TP","PIM","+/-","","POST",
                           "GP","G","A","TP","PIM","+/-","player_id")
player_statistics_col2 <- c('S','TEAM','LEAGUE','GP','GD','GAA','SVS%','GA',
                            'SVS','SO','WLT','TOI','','POST','GP','GD','GAA',
                            'SVS%','GA','SVS','SO','WLT','TOI','player_id')
colnames(player_statistics_all) <- player_statistics_col 
colnames(player_statistics_all2) <- player_statistics_col2

#tournament statistics dataframe
tournament_statistics_all <- data.frame(matrix(ncol = 18, nrow = 0))
tournament_statistics_all2 <- data.frame(matrix(ncol = 24, nrow = 0))
#column information for tournament statistics 
tournament_statistics_col <- c("S","Team","League","GP","G","A","TP","PIM","+/-","","POST",
                           "GP","G","A","TP","PIM","+/-","player_id")
tournament_statistics_col2 <- c('S','TEAM','LEAGUE','GP','GD','GAA','SVS%','GA',
                            'SVS','SO','WLT','TOI','','POST','GP','GD','GAA',
                            'SVS%','GA','SVS','SO','WLT','TOI','player_id')
colnames(tournament_statistics_all) <- tournament_statistics_col 
colnames(tournament_statistics_all2) <- tournament_statistics_col2

#function to manage connection closing and reduce 403 issues
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

#loop through data
for (i in 1:nrow(df2)){
  player_id <- df2$playerID[i]

  player_url <- paste0(df2$player_url[i])
  
  web_page<-read_html(player_url)
  
  #player facts
  #player facts selectors
  dob_pob_selector <- ".col-xs-12.col-17.text-right.p-0.ep-text-color--black"
  other_selector <- ".col-xs-12.col-18.text-right.p-0.ep-text-color--black"
  
  #get date of birth and place of birth information
  dob_info <- html_nodes(web_page,dob_pob_selector) %>% html_text() %>%
      str_trim() %>% # Trim additional white space
      as.character() %>%
      as.data.frame()
  colnames(dob_info) <- c("all_data")
  
  #get remaining player facts information
  nation_info <- html_nodes(web_page,other_selector) %>% html_text() %>%
     str_trim() %>%  #Trim additional white space
     as.character() %>%
     as.data.frame()
  
   colnames(nation_info) <- c("all_data")
  
   player_facts <- data.frame(player_id,dob_info$all_data[1],
                          dob_info$all_data[2],nation_info$all_data[5],
                          nation_info$all_data[7],nation_info$all_data[1],
                          nation_info$all_data[6],nation_info$all_data[8])
   colnames(player_facts) <- player_fact_col
  
   player_facts_all <- rbind(player_facts_all,player_facts)

  #Player statistics" (Games playes etc for every season).
   if (nation_info$all_data[1] != "G"){
      player_statistics_selector <- "table.table-striped.table-condensed.table-sortable.player-stats.skater-stats.highlight-stats"
      player_statistics <- data.frame(matrix(ncol = 18, nrow = 0))
      colnames(player_statistics) <- player_statistics_col
      
      player_statistics <- html_nodes(web_page, player_statistics_selector) %>% html_table(fill=TRUE)
      player_statistics <- do.call(rbind, lapply(player_statistics , as.data.frame))
      if (length(player_statistics) == 17){
        player_statistics$player_id <- player_id
        player_statistics_all <- rbind(player_statistics_all,player_statistics)     
        }

   } else  { #if (nation_info$all_data[1] == "G"
     player_statistics_selector <- "table.table-striped.table-condensed.table-sortable.player-stats.goalie-stats.highlight-stats"
     player_statistics <- data.frame(matrix(ncol = 24, nrow = 0))
     colnames(player_statistics) <- player_statistics_col2
     player_statistics <- html_nodes(web_page, player_statistics_selector) %>% html_table(fill=TRUE)
     player_statistics <- do.call(rbind, lapply(player_statistics , as.data.frame))
     if (length(player_statistics) == 23){
       player_statistics$player_id <- player_id
       player_statistics_all2 <- rbind(player_statistics_all2,player_statistics)
     }

   }
  
    #Tournament statistics
   if(nation_info$all_data[1] != "G"){
     tournament_statistics<- data.frame(matrix(ncol = 18, nrow = 0))
     colnames(tournament_statistics) <- tournament_statistics_col
     tbls <- html_nodes(web_page, "table")
     if (length(tbls) > 3) { # to handle when the tables are not available
       tournament_statistics <- html_nodes(web_page, "table") %>% .[3] %>% html_table(fill=TRUE) # picks the third table
       tournament_statistics <- do.call(rbind, lapply(tournament_statistics , as.data.frame))
       if (length(tournament_statistics) == 17){
         tournament_statistics$player_id <- player_id
         tournament_statistics_all <- rbind(tournament_statistics_all,tournament_statistics)
       }       
     }


     
   } else { #player_id != "227531"
     tournament_statistics<- data.frame(matrix(ncol = 24, nrow = 0))
     colnames(tournament_statistics) <- tournament_statistics_col2
     tbls <- html_nodes(web_page, "table")
     if (length(tbls) > 3) {
       tournament_statistics <- html_nodes(web_page, "table") %>% .[3] %>% html_table(fill=TRUE) # picks the third table
       tournament_statistics <- do.call(rbind, lapply(tournament_statistics , as.data.frame))
       if (length(tournament_statistics) == 23){
         tournament_statistics$player_id <- player_id
         tournament_statistics_all2 <- rbind(tournament_statistics_all2,tournament_statistics) 
       }     
      }

    
   }
   
   CatchupPause(3)

}

#generate csv files
write_csv(player_facts_all,"player_facts_all_.csv")
write_csv(player_statistics_all,"player_statistics_all1_.csv")
write_csv(player_statistics_all2,"player_statistics_all2_.csv")
write_csv(tournament_statistics_all,"tournament_statistics_all1_.csv")
write_csv(tournament_statistics_all2,"tournament_statistics_all12_.csv")














