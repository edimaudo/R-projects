#In order to test and compare the lifetimes of four brands of tyre, 
#you should apply one-way ANOVA method as there is only one factor or criterion (mileage run) 
#to classify the sample observations. 
#The null and alternative hypothesis to be used is given as:

tyre<- read.csv(file.choose(),header = TRUE, sep = ",")
attach(tyre)
is.factor(Brands)
boxplot(Mileage~Brands, main="Fig.-1: Boxplot of Mileage of Four Brands of Tyre", col= rainbow(4))

library(ggplot2)
ggplot(tyre, aes(Brands,Mileage))+
  geom_boxplot(aes(col=Brands))+
  labs(title="Boxplot of Mileage of Four Brands of Tyre")

boxplot.stats(Mileage[Brands=="CEAT"])

#anova
model1<- aov(Mileage~Brands)
summary(model1)
# we do not reject the null hypothesis based on the p-value and f-statistics

#Now you have to find out the pair of brands which differ. 
#For this you may use the Tukey’s HSD test.
#In R, the following are the code for applying the Tukey’s HSD test:
TukeyHSD(model1, conf.level = 0.99)

plot(TukeyHSD(model1, conf.level = 0.99),las=1, col = "red")

par(mfrow=c(2,2))
plot(model1)

#normality chck
uhat<-resid(model1)
shapiro.test(uhat)