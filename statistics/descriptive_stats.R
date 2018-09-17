#remove old data

rm(list=ls())

#load libraries
library(readxl)
library(moments)        # for calculating the skew and kurtosis
library(outliers)       # identifying and extracting outliers
library(ggplot2)

df <- read_excel(file.choose(),2) #baseball data

#central tendency
mean(df$Salary, na.rm = TRUE)

median(df$Salary, na.rm = TRUE)

get_mode <- function(v) {
  unique_value <- unique(v)
  unique_value[which.max(tabulate(match(v, unique_value)))]
}

get_mode(df$Salary)


#range
min(df$Salary)

max(df$Salary)

range(df$Salary)

#percentiles
fivenum(df$Salary)

quantile(df$Salary)

IQR(df$Salary)

#variability
var(df$Salary)
sd(df$Salary)
mad(df$Salary, center = mean(df$Salary))
mad(salaries$Salary, center = median(salaries$Salary)) #median absolute

#shape
skewness(df$Salary, na.rm = TRUE)
kurtosis(df$Salary)


#outliers
# gets most extreme right-tail observation
outlier(df$Salary)

# gets most extreme left-tail observation
outlier(df$Salary, opposite = TRUE)

# observations that are outliers based on z-scores
z_scores <- scores(salaries$Salary, type = "z")
which(abs(z_scores) > 1.96)

ggplot(salaries, aes(x = Position, y = Salary)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()