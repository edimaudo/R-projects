library(fma)

#Monthly total of people on unemployed benefits in Australia (January 1956–July 1992).
mydata <- dole
unemployed_ts <- ts(mydata,start=1956, frequency=12)
plot(unemployed_ts, xlab="Year", ylab="Unemployment benefits")

#Monthly total of accidental deaths in the United States (January 1973–December 1978)
usdeath <- usdeaths
str(usdeath)
usdeath_ts <- ts(usdeath,start=1973, frequency=12)
plot(usdeath_ts, xlab="Year", ylab="US death")

#Quarterly production of bricks (in millions of units) at Portland, Australia (March 1956–September 1994)
bricks <- bricksq
str(bricks)
bricks_ts <- ts(bricks, start=1956, frequency = 4)
plot(bricks_ts, xlab="Year", ylab="Quarterly production of bricks (in millions of units)")


dj <- dowjones
str(dj)
dj_ts <- ts(usdeath,start=1, frequency=1)
plot(dj_ts)


#monthly sales (in thousands) of product A for a plastics manufacturer for years 1 through 5
prod_A_ts = ts(plastics, start = 1, frequency = 1)
plot(prod_A_ts)

fit <- decompose(plastics, type="multiplicative")
plot(fit)