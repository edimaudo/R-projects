#load library
library(rworldmap)
library(tidyverse)

met <- as.data.frame(read.csv(file.choose()))

countries.met <- as.data.frame(table(met$Country))

colnames(countries.met) <- c("country", "value")

matched <- joinCountryData2Map(countries.met, joinCode="NAME", nameJoinColumn="country")

mapCountryData(matched, nameColumnToPlot="value", mapTitle="Met Collection Country Sample", catMethod = "pretty", colourPalette = "heat")

countries.met.2 <- countries.met[-21,]

matched.2 <- joinCountryData2Map(countries.met.2, joinCode="NAME", nameJoinColumn="country")

mapCountryData(matched.2, nameColumnToPlot="value", mapTitle="Met Collection Country Sample", catMethod = "pretty", colourPalette = "heat")

#zoom
mapCountryData(matched.2, nameColumnToPlot="value", mapTitle="Met Collection in Eurasia", mapRegion="Eurasia", colourPalette="heat", catMethod="pretty")