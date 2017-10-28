library(ggplot2)

# example of linear demand curve (first equation) 

demand = function(p, alpha = -40, beta = 500, sd = 10) {
  
  error = rnorm(length(p), sd = sd)
  q = p*alpha + beta + error
  
  return(q)
}

set.seed(100)

prices = seq(from = 5, to = 10, by = 0.1)
q = demand(prices)

data = data.frame('prices' = prices,'quantity' = q)

ggplot(data, aes(prices, quantity)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Demand Curve')


set.seed(10)

hist.prices = rnorm(252, mean = 6, sd = .5) # random prices defined by the company
hist.demand = demand(hist.prices) # demand curve defined in the chunck above
hist.revenue = hist.prices*hist.demand # From the revenue equation
unity.cost = 4 # production cost per unity
hist.cost = unity.cost*hist.demand
hist.profit = (hist.prices - unity.cost)*hist.demand # From the price equation

data = data.frame('Period' = seq(1,252),'Daily.Prices' = hist.prices,
                  'Daily.Demand' = hist.demand, 'Daily.Revenue' = hist.revenue,
                  'Daily.Cost' = hist.cost, 'Daily.Profit' = hist.profit)

ggplot(data, aes(Period, Daily.Prices)) +
  geom_line(color = 4) +
  ggtitle('Historical Prices used for explotation')


ggplot(data, aes(Period, Daily.Revenue, colour = 'Revenue')) +
  geom_line() +
  geom_line(aes(Period, Daily.Profit, colour = 'Profit')) +
  geom_line(aes(Period, Daily.Cost, colour = 'Cost')) +
  labs(title = 'Historical Performance', colour = '')