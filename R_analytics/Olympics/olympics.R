#require(devtools)
#install_version("Rcpp", version = "0.12.19", repos = "http://cran.us.r-project.org")
#country <- unique(df$Country)
#clear old data
rm(list=ls())

#packages
packages <- c("dplyr","ggplot2",'caret','mlbench')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#olympics data
df <- read.csv(file.choose())

#filter for one country - #select country
df_filtered <- df %>%
  dplyr::filter(Country == "Norway")

#age vs sex
ggplot(data=df_filtered, aes(x=Age, na.rm=TRUE)) + geom_bar(aes(fill = Sex)) + xlab("Age") + 
  ggtitle("Age Vs Sex") + theme_bw()

#height vs weight
ggplot(data=df_filtered, aes(x=Height, y=Weight)) +
  geom_point() + xlab("Height") + ylab("Weight") + 
  ggtitle("Weight Vs Height") + theme_bw()

#medal count
df_filtered_medal <- df_filtered %>%
  filter(Medal != "NA")
ggplot(data=df_filtered, aes(x=Medal, na.rm=TRUE)) + geom_bar() + xlab("Medals") +
  ggtitle("Medal Count by podium finish") + theme_bw()

#game and medal count
ggplot(df_filtered_medal, aes(x=factor(Games) , na.rm=TRUE)) + geom_bar(aes(fill = Medal)) + 
  xlab("Olympic games") + 
  ggtitle("Games Medal count") + theme_bw()

#sport and medal count
ggplot(df_filtered_medal, aes(x=Sport , na.rm=TRUE)) + geom_bar(aes(fill = Medal)) + 
  ylab("Sport events") + 
  ggtitle("Sport Medal count") + theme_bw()


##(GDP/Population/Country) to do a regression/clustering model to 
#predict/visualise on results for (summer 2020) olympics..
#have train & test datai
#select method then it would show you the different error metrics like f1 score, accuracy, precision



