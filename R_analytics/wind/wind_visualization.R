#libraries
packages <- c('ggplot2', "tidyverse", 'skimr', 'reshape2', 'lubridate','plotly')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

glimpse(df)

#change column names
colnames(df) <-c("date_time","T1_Possible_Power","T2_Possible_Power","T3_Possible_Power","T4_Possible_Power","T5_Possible_Power","T6_Possible_Power","T7_Possible_Power","T1_Total_Active_Power","T2_Total_Active_Power","T3_Total_Active_Power","T4_Total_Active_Power","T5_Total_Active_Power","T6_Total_Active_Power","T7_Total_Active_Power","mean_wind_mps", "min_wind_mps", "max_wind_mps", "cum_energy_delivered_kwh")

# Data type
Cumulative<-subset(df, select=c(1,19))
Possible<-subset(df, select=1:8)
Active<-subset(df, select=c(1,9:15))
Wind<-subset(df, select=c(1,16:18))
# Data selection
dat<-df 
dim(dat)

#clean up missing data
# Remove missing values
dat<-na.omit(dat)

# energy sentout in each timeblock
n<-length(dat$cum_energy_delivered_kwh)
a<-dat$cum_energy_delivered_kwh[1:n-1]
b<-dat$cum_energy_delivered_kwh[2:n]
diff<-b-a
dat$energy_sentout_10min_kwh<-c(diff,0)
# kinetic energy in the wind at each windspeed
rho=1.225 
area=2174 
turbines=7 
c<-(1/2)*rho*area
dat$wind_power_kw<-c*(dat$mean_wind_mps)^3*turbines/1000 
dat$wind_energy_10min_kwh<-c*(dat$mean_wind_mps)^3*turbines/(1000*6) 
# compute betz limit
betz.coef<- 16/27
dat$betz_limit_10min_kwh<-dat$wind_energy_10min_kwh*betz.coef
# turbine efficiency
dat$turbine_eff<-dat$energy_sentout_10min_kwh/dat$wind_energy_10min_kwh
# total Possible Power
uncurtailed_power<-apply(X=dat[,2:8], MARGIN=1, FUN=sum)
dat$uncurtailed_10min_kwh<-(uncurtailed_power)/6
# curtailment
dat$curtailment_10min_kwh<-dat$uncurtailed-dat$energy_sentout_10min_kwh
# Na due to 0 division
inf<-which(dat$turbine_eff == "Inf")
dat$turbine_eff[inf]<-0

# Make conversion to time date variables
dat$date_time1<-as.POSIXlt(dat$date_time, format="%m/%d/%y")
dat$date_time<-as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M")
# Create date variables
dat$ymd<-ymd(dat$date_time1)
dat$day<-day(dat$date_time1)
dat$week<-week(dat$date_time1)
dat$month<-month(dat$date_time1)
dat$year<-year(dat$date_time1)
dat$yw <- as.numeric(paste(dat$year, dat$week, sep = ""))
dat$hour<-hour(dat$date_time)
# outlier detection
datv<-as.data.frame(lapply(dat[c(2:26)], as.numeric)) # Make variables numeric for cor analysis
colnames(dat)
OUTresult <- scores(datv, type="t", prob=0.95) #t test, probability is 0.95
skim(OUTresult)
# Create energy data
energy<-subset(dat, select=c("ymd", "energy_sentout_10min_kwh", "wind_energy_10min_kwh", "betz_limit_10min_kwh", "uncurtailed_10min_kwh", "curtailment_10min_kwh"))
energy<-ddply(energy, .(ymd), numcolwise(sum))

# Visualization energy & time
test<-melt(energy, id.vars=("ymd"))
test<-na.omit(test)
# Remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.025, .975), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)]  (qnt[2] + H)] <- NA
y}
testo <- as.data.frame(remove_outliers(test$value))
names(testo)[names(testo) == "remove_outliers(test$value)"] <- "valueo"
test1<-cbind(test,testo)