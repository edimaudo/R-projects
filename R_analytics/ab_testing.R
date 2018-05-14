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

#simple sample
s <- sample(groups, length(groups), FALSE)
diff(by(data, s, mean))

#perform multiple sampling
max.iter <- 10000
examples <- unlist(lapply(1:max.iter, function(x) {
  diff(by(data, sample(groups, length(groups), FALSE), mean))  
}))

#plot
test.diff <- diff(by(data, groups, mean))
par(mfrow=c(1,1))
hist(examples, col = "red", breaks = 100, main="Random Permutations", xlab="")
abline(v = test.diff, col = "black", lwd = 4)

#compute p value
# one-tailed test
(sum(examples > test.diff) + 1) / (max.iter + 1)  


# two-tailed test
(sum(abs(examples) > abs(test.diff)) + 1) / (max.iter + 1)  
