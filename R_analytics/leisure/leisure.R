#remove old data
rm(list=ls())
#packages
packages <- c("ggplot2","pastecs")
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.table(file.choose(), header=T)

glimpse(df)

#dewscriptive statistics
stat.desc(df[,2:6])

ggplot(df, aes(x=lifesat, y=leissat)) + geom_point() + geom_point(size=2, shape=23) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + theme_classic()

ggplot(data=df, aes(x=jobstres)) +
  geom_bar() + ylab("Count") + 
  ggtitle("Jobstres description") + theme_classic()

ggplot(data=df, aes(x=jobstres)) +
  geom_bar(aes(fill=famstres)) + ylab("Count") + 
  ggtitle("Jobstres description") + theme_classic()

ggplot(df, aes(x=leissat)) + geom_histogram() + theme_classic()

ggplot(df, aes(x=leissat)) + geom_density() + theme_classic()