#remove old data
rm(list=ls())


#libraries
library(tidyverse)
library(reshape2)

# retention rate data
df_ret <- data.frame(month_lt = c(0:7),
                     case01 = c(1, .531, .452, .423, .394, .375, .356, .346),
                     case02 = c(1, .869, .743, .653, .593, .551, .517, .491),
                     case03 = c(1, .677, .562, .486, .412, .359, .332, .310),
                     case04 = c(1, .631, .468, .382, .326, .289, .262, .241)
) %>%
  melt(., id.vars = c('month_lt'), variable.name = 'example', value.name = 'retention_rate')

ggplot(df_ret, aes(x = month_lt, y = retention_rate, group = example, color = example)) +
  theme_minimal() +
  facet_wrap(~ example) +
  scale_color_manual(values = c('#4e79a7', '#f28e2b', '#e15759', '#76b7b2')) +
  geom_line() +
  geom_point() +
  theme(plot.title = element_text(size = 20, face = 'bold', vjust = 2, hjust = 0.5),
        axis.text.x = element_text(size = 8, hjust = 0.5, vjust = .5, face = 'plain'),
        strip.text = element_text(face = 'bold', size = 12)) +
  ggtitle('Retention Rate')

# functions for sBG distribution
churnBG <- Vectorize(function(alpha, beta, period) {
  t1 = alpha / (alpha + beta)
  result = t1
  if (period > 1) {
    result = churnBG(alpha, beta, period - 1) * (beta + period - 2) / (alpha + beta + period - 1)
  }
  return(result)
}, vectorize.args = c("period"))

survivalBG <- Vectorize(function(alpha, beta, period) {
  t1 = 1 - churnBG(alpha, beta, 1)
  result = t1
  if(period > 1){
    result = survivalBG(alpha, beta, period - 1) - churnBG(alpha, beta, period)
  }
  return(result)
}, vectorize.args = c("period"))

MLL <- function(alphabeta) {
  if(length(activeCust) != length(lostCust)) {
    stop("Variables activeCust and lostCust have different lengths: ",
         length(activeCust), " and ", length(lostCust), ".")
  }
  t = length(activeCust) # number of periods
  alpha = alphabeta[1]
  beta = alphabeta[2]
  return(-as.numeric(
    sum(lostCust * log(churnBG(alpha, beta, 1:t))) +
      activeCust[t]*log(survivalBG(alpha, beta, t))
  ))
}

df_ret <- df_ret %>%
  group_by(example) %>%
  mutate(activeCust = 1000 * retention_rate,
         lostCust = lag(activeCust) - activeCust,
         lostCust = ifelse(is.na(lostCust), 0, lostCust)) %>%
  ungroup()

ret_preds01 <- vector('list', 7)
for (i in c(1:7)) {
  
  df_ret_filt <- df_ret %>%
    filter(between(month_lt, 1, i) == TRUE & example == 'case01')
  
  activeCust <- c(df_ret_filt$activeCust)
  lostCust <- c(df_ret_filt$lostCust)
  
  opt <- optim(c(1, 1), MLL)
  retention_pred <- round(c(1, survivalBG(alpha = opt$par[1], beta = opt$par[2], c(1:7))), 3)
  
  df_pred <- data.frame(month_lt = c(0:7),
                        example = 'case01',
                        fact_months = i,
                        retention_pred = retention_pred)
  ret_preds01[[i]] <- df_pred
}

ret_preds01 <- as.data.frame(do.call('rbind', ret_preds01))