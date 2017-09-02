##spring leaf data
setwd("/Users/edima/Documents/Coding/R")
springleaf <- read.csv(file = "springleaf.csv")
nrow(springleaf)
ncol(springleaf)
nrow(springleaf)*ncol(springleaf)
summary(springleaf[,1:10])
ids <- length (springleaf$ID)

#data check
complete.cases (springleaf)
table (complete.cases (springleaf))
str (springleaf$VAR_0237)
table ((springleaf$VAR_0237) %in% c("CA", "NY", "NJ"))
springleaf$VAR_0237 %in% c("CA", "NY", "NJ")
springleaf$IsLiberal <- springleaf$VAR_0237 %in% c("CA", "NY", "NJ")

#removing bad data
table (complete.cases (springleaf))

table(is.na (springleaf$VAR_0207))
table(is.na (springleaf$VAR_0213))
table(is.na (springleaf$VAR_0840))

springleaf$VAR_0207 <- NULL
springleaf$VAR_0213 <- NULL
springleaf$VAR_0840 <- NULL

mean (springleaf$VAR_0006)
mean (springleaf$VAR_0006, na.rm = TRUE)

#basic data analysis
mean (springleaf$VAR_0583)
median (springleaf$VAR_0583)
quantile (springleaf$VAR_0583)

a <- springleaf$VAR_0583
table (cut (a, c(-Inf, mean (a) - 1.5*sd(a), mean(a)+1.5*sd(a), Inf)))
springfield$VAR_0583 [springfield$VAR_0583 > 8.91e+08] <- NA
quantile (springfield$VAR_0583, na.rm = TRUE)

#data stratification
validationIDs <- sample (springfield$ID, nrow(springfield)*.75)
validation <- springfield[springfield$ID %in% validationIDs,]
test <- springfield[!(springfield$ID %in% validationIDs),]
prop.table (table (validation$target))
prop.table (table (test$target))
table (springfield$ID %in% c(test$ID, validation$ID))

#summary
BELOW_1st <- (springleaf$VAR_0004 < summary (springleaf$VAR_0004) [1])
ABOVE_3rd <- (springleaf$VAR_0004 < summary (springleaf$VAR_0004)[5])
VAR_0004_OUTSIDE_IQR_TF <- ifelse (BELOW_1st |ABOVE_3rd,TRUE,FALSE)
springleaf <- cbind.data.frame(springleaf, VAR_0004_OUTSIDE_IQR_TF)

