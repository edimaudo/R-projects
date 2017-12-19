#remove all data
rm(list=ls())

#use filechooser to select csv file
mydata <- read.csv(file.choose(), sep = ',')

str(mydata)

summary(mydata)

library("ggpubr")
ggboxplot(mydata, x = "material", y = "gpi", color = "group",
          palette = c("#00AFBB", "#E7B800","#00ABBB","#E7E800","#00AFBF","#E7F800"))

res.aov2 <- aov(gpi ~ material + group, data = mydata)
summary(res.aov2)

# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(gpi ~ material + group, data = mydata)
res.aov3 <- aov(gpi ~ material + group + material:group, data = mydata)
summary(res.aov3)

# 1. Homogeneity of variances
plot(res.aov3, 1)

# 2. Normality
plot(res.aov3, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )