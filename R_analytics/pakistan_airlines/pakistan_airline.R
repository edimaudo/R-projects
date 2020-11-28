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

#create player facts data frame
player_facts_all <- data.frame(matrix(ncol = 8, nrow = 0))
#column information for player facts
player_fact_col <- c("player_id","date_of_birth",
                     "place_of_birth","nation",
                     "youth_team","position",'shoots','status')
colnames(player_facts_all) <- player_fact_col