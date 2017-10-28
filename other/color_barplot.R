#barplots with color

x <- c(3,5,12,13, 10, 8)
#name
barplot(x,col="slategray3")
#rgb
barplot(x, col = rgb(.54,.0,.0))
#hex
barplot(x,col="#212121")
#color cycle
barplot(x, col=c("red","blue","green"))

help(package = colorspace)
palette()
barplot(x, col = 1:6)
barplot(x, col = rainbow(6))
barplot(x, col = heat.colors(6))
barplot(x, col = terrain.colors(6))
barplot(x, col = topo.colors(6))
barplot(x, col = cm.colors(6))
palette("default")  # Return to default

rm(list = ls())  # Clean up