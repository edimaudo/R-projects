#remove old data
rm(list=ls())

#load libraries
library(tidyverse)
library(caret)
library(mlbench)
library(data.table)
library(corrplot)
library(ggplot2)

#load data
hd1 <- read.csv(file.choose()) #load housing header text

View(hd1)

#pairwise correlation
corrplot(cor(hd1),'number')

library(lattice)
library(car)
#scatterplot matrix
scatterplotMatrix(~Zn+Indus+Chas+Nox+Rm+Age+Dis+Rad+Tax+Ptratio+B+Lstat|Medv, data=mtcars,
                   main="Medv Options")