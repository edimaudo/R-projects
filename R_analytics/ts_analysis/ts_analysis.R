# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','SnowballC')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}