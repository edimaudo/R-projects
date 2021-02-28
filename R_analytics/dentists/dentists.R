#===================
# Objective
#===================
#objective of this project is to conduct statistical analyses 
#to address a public health topic using appropriate statistical methods
 
#===================
# Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",'aod',
              'readxl','car','lmtest')

# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
# Load Data
#===================
setwd("~/Documents/Coding/R/R_analytics/dentists")
df <- read_excel("Data Set Dentists BioStats-2.xlsx",sheet = "Data")
colnames(df) <- c('Country','dentist_number','Target')

# get data summary
summary(df)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

set.seed(2020)
# build logistic regression model
df$Target <- as.factor(df$Target)
df$Country <- as.factor(df$Country)

Country <- df %>%
  select(Country)

Country2 <- as.data.frame(lapply(Country, labelEncoder))

model <- glm(Target ~ dentist_number, data = df, family = "binomial")

# model details
summary(model)

# confidence interval
confint(model)

## CIs using standard errors
confint.default(model)

# exponentiated coefficients
exp(model$coefficients)        

# 95% CI for exponentiated coefficients
exp(confint(model))             

#Analysis of variance for individual terms
Anova(model, type="II", test="Wald")

#Wald test
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)

# Overall p-value for model
anova(model,
      update(model, ~1),    # update here produces null model for comparison
      test="Chisq")

# likelihood ratio test
lrtest(model)

# Plot of standardized residuals
plot(fitted(model),rstandard(model))

#Plotting the model
plot(Target ~ dentist_number,data = df,xlab="Dentist number",ylab="Essential",pch=19)             

curve(predict(model,data.frame(dentist_number=x),type="response"),
      lty=1, lwd=2, col="blue", add=TRUE)


#Questions to Address

# What is Your Public Health Topic?
# The objective is to understand oral health care across the globe
# from across the world
  
## What is the research question?
# Understanding the relationship between oral health plans and dentists
  
## What is the Null Hypothesis?
#The null hypothesis is that  Number of Dentists per 10,000 residents	
#has an impact on Existence of operational policy/strategy/action plan for oral health 
  
#What is the Alternative Hypothesis?
#Number of Dentists per 10,000 residents	
#does not have an impact on Existence of operational policy/strategy/action plan for oral health 
  
#What Research Method will be used?(Quantitative or Qualitative?)
#Quantitative since the key variables are Quantitative


#What Research Study Design will be used?
#Logistic regession will be applied because 
    
#How Many Samples will be used? (One, Two, or More?)
#One sample

#What level of alpha would you set to test your hypothesis? 
#95%
  
##Can you address how to avoid Type I and/or Type II errors? 
#The probability of a type 1 error (rejecting a true null hypothesis) can be minimized by picking a smaller level of significance 
#α before doing a test (requiring a smaller p-value for rejecting H0).

#Once the level of significance is set, the probability of a type 2 error 
#(failing to reject a false null hypothesis) can be minimized either by picking a larger sample size or 
#by choosing a "threshold" alternative value of the parameter in question that is further from the 
#null value
  
##Are you able to calculate a point estimate and/or a confidence interval with the information given?
#Yes - see model output
  
##Why did you pick the above descriptive and inferential statistics? 
#Inferential statictics was utlized since it would help in giving better
#insight into the research question
  