#remove all data
rm(list=ls())

mydata <- econsumption

str(mydata)

#linear regression
plot(Mwh ~ temp, data=econsumption)
fit <- lm(Mwh ~ temp, data=econsumption)
plot(residuals(fit) ~ temp, data=econsumption)
forecast(fit, newdata=data.frame(temp=c(10,35)))

mydata2 <- olympic
plot(time ~ Year, data=olympic)

fit <- lm(time ~ Year, data = olympic)
abline(fit)
summary(fit)
plot(residuals(fit) ~ Year, data=olympic)
forecast(fit, newdata=data.frame(temp=c(2000,2004,2008, 2012)))