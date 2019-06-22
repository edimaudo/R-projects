#remove old data
rm(list=ls())
#packages
packages <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
              "cluster", "igraph", "fpc")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

