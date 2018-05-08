

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

#cause effect diagram aka ishikawa/fishbone
# Cause and Effect Diagram
##Create effect as string
effect <- "Low Quality product"
##Create vector of causes
causes.gr <- c("Measurement", "Material", "Methods", "Environment",
               "Manpower", "Machines")
# Create indiviual cause as vector of list
causes <- vector(mode = "list", length = length(causes.gr))
causes[1] <- list(c("Lab error", "Contamination"))
causes[2] <- list(c("Raw Material", "Additive"))
causes[3] <- list(c("Sampling", "Analytical Procedure"))
causes[4] <- list(c("Rust near sample point"))
causes[5] <- list(c("Poor analyst","No guidance"))
causes[6] <- list(c("Leakage", "breakdown"))
# Create Cause and Effect Diagram
ss.ceDiag(effect, causes.gr, causes, sub = "Fish Bone Diagram Example")



#gage analysis
# Create gage R and R data for 3 piston rings , 2 operators and each operator 3 measurements per piston
Operator<- factor(rep(1:2, each = 9))
Pistonring<- factor(rep(rep(1:3, each = 3), 2))
run<- factor(rep(1:3, 6))
diameter<-c(1.4727, 1.4206, 1.4754, 1.5083, 1.5739,
            1.4341, 1.5517, 1.5483, 1.4614, 1.3337,
            1.6078, 1.4767, 1.4066, 1.5951, 1.8419,
            1.7087, 1.8259, 1.5444)
pistondata<-data.frame(Operator,Pistonring,run,diameter)

#Load package
library("SixSigma")

#Perform gage R & R
my.rr <- ss.rr(var = diameter, part = Pistonring,
               appr = Operator,
               data = pistondata,
               main = "Six Sigma Gage R&R Measure",
               sub = "Piston ring MSA")