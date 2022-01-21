##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

