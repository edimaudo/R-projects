library(tidyverse)
library(neuralnet)
library(GGally)

url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases//haberman/haberman.data'

Hab_Data <- read_csv(file = url,
                     col_names = c('Age', 'Operation_Year', 
                                   'Number_Pos_Nodes','Survival')) %>%
  na.omit() %>%
  mutate(Survival = ifelse(Survival == 2, 0, 1),
         Survival = factor(Survival))


#explore data
ggpairs(Hab_Data, title = "Scatterplot Matrix of the Features of the Haberman's Survival Data Set")

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

Hab_Data <- Hab_Data %>%
  mutate(Age = scale01(Age), 
         Operation_Year = scale01(Operation_Year), 
         Number_Pos_Nodes = scale01(Number_Pos_Nodes), 
         Survival = as.numeric(Survival)-1)

Hab_Data <- Hab_Data %>%
  mutate(Survival = as.integer(Survival) - 1, 
         Survival = ifelse(Survival == 1, TRUE, FALSE))

#1 hidden layer 1 neuron ANN
set.seed(123)
Hab_NN1 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                     data = Hab_Data, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = TRUE)

plot(Hab_NN1, rep = 'best')


#check errors
Hab_NN1_Train_Error <- Hab_NN1$result.matrix[1,1]
paste("CE Error: ", round(Hab_NN1_Train_Error, 3)) 

Hab_NN1_AIC <- Hab_NN1$result.matrix[4,1]
paste("AIC: ", round(Hab_NN1_AIC,3))

Hab_NN2_BIC <- Hab_NN1$result.matrix[5,1]
paste("BIC: ", round(Hab_NN2_BIC, 3))


set.seed(123)
# 2-Hidden Layers, Layer-1 2-neurons, Layer-2, 1-neuron
Hab_NN2 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                     data = Hab_Data, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = 
                       TRUE, hidden = c(2,1))

# 2-Hidden Layers, Layer-1 2-neurons, Layer-2, 2-neurons
set.seed(123)
Hab_NN3 <- Hab_NN2 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                                data = Hab_Data, 
                                linear.output = FALSE, 
                                err.fct = 'ce', 
                                likelihood = TRUE, 
                                hidden = c(2,2))

# 2-Hidden Layers, Layer-1 1-neuron, Layer-2, 2-neuron
set.seed(123)
Hab_NN4 <- Hab_NN2 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                                data = Hab_Data, 
                                linear.output = FALSE, 
                                err.fct = 'ce', 
                                likelihood = TRUE, 
                                hidden = c(1,2))

# Bar plot of results
Class_NN_ICs <- tibble('Network' = rep(c("NN1", "NN2", "NN3", "NN4"), each = 3), 
                       'Metric' = rep(c('AIC', 'BIC', 'ce Error * 100'), length.out = 12),
                       'Value' = c(Hab_NN1$result.matrix[4,1], Hab_NN1$result.matrix[5,1], 
                                   100*Hab_NN1$result.matrix[1,1], Hab_NN2$result.matrix[4,1], 
                                   Hab_NN2$result.matrix[5,1], 100*Hab_NN2$result.matrix[1,1],
                                   Hab_NN3$result.matrix[4,1], Hab_NN3$result.matrix[5,1], 
                                   100*Hab_NN3$result.matrix[1,1], Hab_NN4$result.matrix[4,1], 
                                   Hab_NN4$result.matrix[5,1], 100*Hab_NN4$result.matrix[1,1]))

Class_NN_ICs %>%
  ggplot(aes(Network, Value, fill = Metric)) +
  geom_col(position = 'dodge')  +
  ggtitle("AIC, BIC, and Cross-Entropy Error of the Classification ANNs", "Note: ce Error displayed is 100 times its true value")
