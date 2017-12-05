#statistics

#t-test
#for example, if we wanted to test whether the volume of a 
#shipment of lumber was less than usual (μ0=39000μ0=39000 cubic feet),
set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: %>%  mu = 39000

#paired t-test
# For instance, let’s say that we work at a large health clinic and we’re testing a new drug, 
# Procardia, that’s meant to reduce hypertension. We find 1000 individuals with a 
# high systolic blood pressure (x¯=145x¯=145mmHg, SD=9SD=9mmHg), we give them Procardia for a month, 
# and then measure their blood pressure again. 
# We find that the mean systolic blood pressure has decreased to 138mmHg with a standard deviation 8mmHg.
set.seed(2820)

preTreat <- c(rnorm(1000, mean = 145, sd = 9))
postTreat <- c(rnorm(1000, mean = 138, sd = 8))

t.test(preTreat, postTreat, paired = TRUE)

#indepdent t test
set.seed(0)

ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)

t.test(ClevelandSpending, NYSpending, var.equal = TRUE)

#binary data
spending <- c(ClevelandSpending, NYSpending)
city <- c(rep("Cleveland", 50), rep("New York", 50))

t.test(spending ~ city, var.equal = TRUE)

#unequal variance
ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)
t.test(ClevelandSpending, NYSpending, var.equal = FALSE)