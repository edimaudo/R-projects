#rfm model

#clear old data
rm(list=ls())

#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl',
              'lubridate','')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load excel file
df <- as.data.frame(read_excel(file.choose()))

glimpse(df)

#rename columns
colnames(df) <- c("TRANDATE","SALESTXN","CUSTNO")

#do rfm model
rfm_data <- df %>%
  select(CUSTNO,TRANDATE,SALESTXN) %>%
  drop_na()

#convert date
rfm_data <- rfm_data %>%
  mutate(TRANDATE = as.Date(TRANDATE,"%Y-%m-%d"))

#create real rfm data
rfm_temp <- rfm_data %>%
  group_by(CUSTNO) %>%
  summarise(recency=(as.numeric(as.Date(today())-max(TRANDATE))),
            frequency=n(), monetary= sum(SALESTXN))

#rfm rank scoring

# sort data set for Recency with Recency (ascending) - Frequency (descending) - Monetary (descending)
rfm_temp <- rfm_temp %>%
  arrange(recency, desc(frequency), desc(monetary))
rfm_temp <- rfm_temp %>%
  mutate(recency = 0-recency)
rfm_temp$rankR <- cut(rfm_temp$recency,5,labels=F)
#change recency back to normal
rfm_temp <- rfm_temp %>%
  mutate(recency = 0-recency)

# sort data set for Frequency with Recency (descending) - Frequency (ascending) - Monetary (descending)
rfm_temp <- rfm_temp %>%
  arrange(desc(recency), frequency, desc(monetary))
rfm_temp$rankF <- cut(rfm_temp$frequency,5,labels=F) 

# sort data set for Monetary with Recency (descending) - Frequency (descending) - Monetary (ascending)
rfm_temp <- rfm_temp %>%
  arrange(desc(recency), desc(frequency), monetary)
rfm_temp$rankM <- cut(rfm_temp$monetary,5,labels=F)

#combine rfm into single score
rfm_temp$Score <- with(rfm_temp, paste0(rankR, rankF, rankM))
rfm_temp <- rfm_temp %>%
  mutate(Score = as.integer(Score))

#get output sorted by score
rfm_temp <- rfm_temp %>%
  arrange(desc(Score))