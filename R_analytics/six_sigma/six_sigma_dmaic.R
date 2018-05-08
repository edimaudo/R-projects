

#Load package
library("SixSigma")

#process map
# Create vector of Input , Output and Steps  
inputs <-c ("Ingredients", "Cook", "Oven")
outputs <- c("temperature", "taste", "tenderness","weight", "radius", "time")
steps <- c("DOUGH", "TOPPINGS", "BAKE", "DELIVER")

#Save the names of the outputs of each step in lists 
io <- list()
io[[1]] <- list("X's")
io[[2]] <- list("Dough", "ingredients", "Cooker")
io[[3]] <- list("Raw Pizza", "Cooker", "Oven Plate")
io[[4]] <- list("Baked Pizza", "Plate")

#Save the names, parameter types, and features:
param <- list()
param[[1]] <- list(c("Cook", "C"),c("flour brand", "C"),c("prop Water", "P"))
param[[2]] <- list(c("Cook", "C"),c("Ing.Brand", "Cr"),c("amount", "P"),c("prep.Time", "Cr"))
param[[3]] <- list(c("Cook","C"),c("queue", "N"),c("BakeTime", "Cr"))
param[[4]] <- list(c("Waiter","C"),c("queue", "N"))

feat <- list()
feat[[1]] <- list("Density", "toughness", "thickness")
feat[[2]] <- list("Diameter", "Weight", "thickness")
feat[[3]] <- list("temperature", "tenderness", "taste")
feat[[4]] <- list("temperature", "taste", "tenderness","weight", "time")
# Create process map
ss.pMap(steps, inputs, outputs,io, param, feat,sub = "Pizza Process Example")