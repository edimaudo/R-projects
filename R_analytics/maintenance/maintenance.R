#libraries
library(dplyr)
library(ggplot2)
library(survival)

#goal - Can you predict which machines will break next?
#The solution will predict which machine will break next and propose 
#some reasons about why these machines have different lifetimes. 
#A survival analysis is a good choice as we can visualise the machine’s lifetime very easily.

#load data
mydata <- read.csv("maintenance_data.csv")

#summary
summary(mydata)

#exploration of data
## When do machine breack on average?
maintenance_broken <- mydata %>% filter(broken == 1)
par(mfrow=c(1,3))
boxplot(lifetime~broken,data=maintenance_broken, main="Broken machines", xlab="", 
        ylab="Lifetime",col="#357EC7")
boxplot(lifetime~team,data=maintenance_broken, main="Per team", xlab="", ylab="",col="#357EC7")
boxplot(lifetime~provider,data=maintenance_broken, main="Per provider", xlab="", ylab="",col="#357EC7")

#survival analysis
dependantvars = Surv(mydata$lifetime, mydata$broken)

# Create model (use the gaussian method)
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, 
                  dist="gaussian",data=mydata)
print(survreg)

#visualize survival
library(GGally)
maintenance_graph <- survfit(Surv(lifetime,broken) ~ 1, data = mydata)
ggsurv(maintenance_graph)

#per team
maintenance_graph2 <- survfit(Surv(lifetime,broken) ~ team, data = mydata)
ggsurv(maintenance_graph2)

#provider
maintenance_graph3 <- survfit(Surv(lifetime,broken) ~ provider, data = mydata)
ggsurv(maintenance_graph3)

#summary
summary(surveg)

#prediction
# Predict
# p = percentile = 0,5 = expected median
Ebreak=predict(survreg, newdata=mydata, type="quantile", p=.5)

# Make forecast
Forecast=data.frame(Ebreak)
Forecast$lifetime=mydata$lifetime
Forecast$broken=mydata$broken

# Computed Expected Remaining Lifetime (remainingLT)
Forecast$RemainingLT=Forecast$Ebreak-mydata$lifetime

# Order the elements by Expected Remaining Lifetime
Forecast=Forecast[order(Forecast$RemainingLT),]

# Keep only those who are not broken yet
ActionsPriority=Forecast[Forecast$broken==0,]
ActionsPriorityDT <- head(ActionsPriority, n=20)
