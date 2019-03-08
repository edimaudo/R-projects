#remove old data
rm(list=ls())

#load data
diet_df <- read.csv(file.choose()) #diet data

#diet summary
summary(diet_df)

#identify outliers for HBLevel and RBC Count
ggplot(diet_df, aes(x=HbLevel, y=RBC)) + geom_boxplot()

ggplot(diet_df, aes(RBC)) + geom_boxplot() 

#find age quartiles
quantile(diet_df$Age)

#age histogram
ggplot(diet_df, aes(Age)) + geom_histogram(binwidth = 7) + theme_classic()

#diet
ggplot(diet_df, aes(Diet)) + geom_bar() + theme_classic() 

counts <- table(diet_df$Diet, diet_df$Race)
barplot(counts, main="Diet by Race",
        xlab="Diet", col=c("darkblue","red"),
        legend = rownames(counts))
