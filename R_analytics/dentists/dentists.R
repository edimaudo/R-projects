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

# get data summary
summary(df)

colnames(df) <- c('Country','dentist_number','Target')

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

set.seed(2020)
# build logistic regression model
df$Target <- factor(df$Target)
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

What is Your Public Health Topic?
  
What is the research question?
  
What is the Null Hypothesis?
  
What is the Alternative Hypothesis?
  
What Research Method will be used?(Quantitative or Qualitative?)

What Research Study Design will be used?
  
How Many Samples will be used? (One, Two, or More?)

Identify the Variables That May Be Used in the Study
Identify the Level of Measurement for Each Variable That Will Be Used in the Study
(Nominal, Ordinal, Interval, or Ratio)	
List All Descriptive Statistics That Will Be Used 	
List All Inferential Statistics That May Be Used

What level of alpha would you set to test your hypothesis? 
  
Can you address how to avoid Type I and/or Type II errors? 
  
Are you able to calculate a point estimate and/or a confidence interval with the information given?
  
Why did you pick the above descriptive and inferential statistics? 
  