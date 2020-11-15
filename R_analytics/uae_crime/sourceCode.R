#===================
## Load Libraries:
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",
              'doParallel','extraTrees','FactoMineR','factoextra','readxl','scales',
              'gridExtra','lubridate')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
## Load Data
#===================
crimes_data <- read.csv("number-of-crimes-committed-against-persons-disturbing-crimes-by-sex-age-marital-status-etc.csv", sep = ";")%>%
  rename(year = ?..Year) %>% # This can be removed according to the names given to Year after loading the dataset
  mutate(Age = as.numeric(Age)) %>% # Convert age to numerical
  dplyr::select(-Description.of.the.charge) %>% # Remove the description of the charge variable
  mutate_if(is.character, as.factor) # Convert character type variable to factor type

# Show the structure of the dataset
str(crimes_data)

#===================
## Data exploration
#===================

# png("boxplot_age.png")
boxplot(crimes_data$Age, horizontal = TRUE, col = "lightblue", main = "Boxplot of the Age")
# dev.off()

# Data visualization:
# png("crimes_gender.png")
crimes_data %>%
  group_by(Gender) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = Gender, y = Number, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by gender")
# dev.off()


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>% # Graph showing the number of crimes by the marital status
  group_by(Marital.Status) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Marital.Status, Number), y = Number, fill = Marital.Status)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Marital Status") + 
  xlab("Marital Status") + 
  ylab("Number of crimes")

# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(Educational.Level) %>% # Graph showing the number of crimes by the Educational level
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Educational.Level, Number), y = Number, fill = Educational.Level)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Educational Level") + 
  xlab("Educational Level") + 
  ylab("Number of crimes")


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(Religion) %>% # Graph showing the number of crimes by religion
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = reorder(Religion, Number), y = Number, fill = Religion)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Religion") + 
  xlab("Religion") + 
  ylab("Number of crimes")


# png("crimes_jobs.png")
crimes_data %>%
  group_by(Job) %>%
  summarize(Number = sum(Number)) %>%
  arrange(desc(Number)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(Job, Number), y = Number, fill = Job)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by Job") + 
  xlab("Job") + 
  ylab("Number of crimes")
# dev.off()


# This is not added to the report, we only added graphs with the good added value:
crimes_data %>%
  group_by(crime) %>% # Graph showing the number of crimes by crime type
  summarize(Number = sum(Number)) %>%
  arrange(desc(Number)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(crime, Number), y = Number, fill = crime)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ggtitle("Number of crimes by crime type") + 
  xlab("Crime type") + 
  ylab("Number of crimes")




# Display the number of crimes over the years
png("evolution_crimes.png")
crimes_data %>%
  group_by(year) %>%
  summarize(Number = sum(Number)) %>%
  ggplot(aes(x = year, y = Number)) + 
  geom_line(color = "#6d031c") +
  geom_point(color = "#a79086")+
  scale_x_continuous(breaks = seq(2012, 2019, 1)) + 
  scale_y_continuous(breaks = seq(200, 1000, 200)) + 
  guides(fill = FALSE) + 
  ggtitle("Evolution of the number of crimes") + 
  xlab("Year") + 
  ylab("Number of crimes")
dev.off()


# Show the crimes related to which jobs
crimes_data_job <- crimes_data %>%
  mutate(Job = as.character(Job)) %>%
  filter(Job %in% c("Not Specified", "Employee", "Worker", "Student", "Driver", "House Wife"))

# Plot the heatmap
# png("heatmap.png")
pheatmap::pheatmap(data.matrix(table(crimes_data_job$Job, crimes_data_job$crime)),
                   treeheight_row = 0,
                   treeheight_col = 0)

#===================
## Data processing
#===================
crimes_data_model <- crimes_data %>%
  dummy_cols(c("Gender", "Marital.Status", 
               "Educational.Level", "Religion", "Job"),
             remove_first_dummy = TRUE) %>%
  dplyr::select(-c("Gender", "Marital.Status", "Educational.Level",
                   "Religion", "Job", "Number", "year")) %>%
  na.omit


str(crimes_data_model)

#-------------
# RFE Method
#-------------

registerDoSEQ()
control <- rfeControl(functions=rfFuncs,
                      method="cv", 
                      number=10)
# run the RFE algorithm
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
results <- rfe(crimes_data_model %>% select(-crime), 
               crimes_data_model$crime, sizes=c(1:8),
               rfeControl=control)
stopCluster(cl)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
# png("plot.png")
plot(results, type=c("g", "o"))
# dev.off()

#-------------
# PCA Analysis
#-------------



# dev.off()