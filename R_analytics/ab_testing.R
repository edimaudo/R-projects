#We run an A/B test with:
  
#Original checkout page
#Checkout page with widget

set.seed(3) 
grp.1 <- round(rlnorm(100, 6)) # With Widget
grp.2 <- round(rlnorm(100, 6)) # Original page

#plot histograms
hist(grp.1)
hist(grp.2)

#difference in groups
groups <- c(rep(1, length(grp.1)), rep(2, length(grp.2)))
data <- c(grp.1, grp.2)

diff(by(data, groups, mean))