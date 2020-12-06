# Goal is to perform regression modelling
#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment
# libraries
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','scales',
              'dplyr','mlbench','caTools')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose()) #lease data

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) #no missing data

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# data cleaning
cat_var_recode <- df %>%
  select(Location,Restaurant, Wiring, Exercise, FirmType, Renewable)
cat_var <- df %>%
  select(Renovation, LeaseLen, FloorsBldg, Elevators, FloorLease, Parking)
cts_var <- df %>%
  select(SqftLease, Age, DistCity, DistAirp, DriveAirp, Occupancy, SqftFloor, DistHosp)
Target <- df %>%
  select(RentTotal)

# recode Location, Restaurant, Wiring, Exercise, FirmType, Renewable
cat_var_recode <- as.data.frame(lapply(cat_var_recode, labelEncoder))

#updated regression model with standardized cts variables
#cts_var <- as.data.frame(lapply(cts_var, normalize))

#combine columns
new_df <- cbind(cat_var_recode, cat_var, cts_var, Target)

#correlation
corrplot(cor(new_df), method = "num")

#regression model
fit <- lm(RentTotal ~., data=new_df)
summary(fit) # show results
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics
AIC(fit)
BIC(fit)
library(car)
car::vif(fit) #find variables to remove.  if high might be worth removing

library(Metrics)
predictions <- fit %>% predict(new_df)
rmse_cal <- rmse(new_df$RentTotal,predictions)

plot(fit)
