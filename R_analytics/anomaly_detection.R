
#Bitcoin Price Anomaly Detection

library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk

#extract data
btc <- get_historic_price(start = "2018-01-01")

#data preprocessing
btc_ts <- btc %>% rownames_to_column() %>% as.tibble() %>% 
  mutate(date = as.Date(rowname)) %>% select(-one_of('rowname'))

#time series decomposition
btc_ts %>% 
  time_decompose(Price, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

#anomaly detection
btc_ts %>% 
  time_decompose(Price) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

btc_ts %>% 
  time_decompose(Price) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
