# LIME installation
# install.packages("devtools", repos='http://cran.us.r-project.org')
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
# devtools::install_github("merrillrudd/LIME", dependencies=TRUE)
# devtools::install_github("tokami/TropFishR")

#######################################
# clear environment
#######################################
rm(list = ls()) 
#######################################
# Packages
#######################################
packages <- c('ggplot2','corrplot','tidyverse','readxl',
              'LIME','TMB','TMBhelper','LBSPR')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#######################################
# Load data
#######################################
df<- read.csv(file.choose(), header=TRUE)
#######################################
# LH information
#######################################
# single fleet
lh <- create_lh_list(vbk = 1.50, linf = 24.5, t0 = 0, lwa = 0.00407, lwb = 3.16,
                     S50 = c(5.66), S95 = c(7.98), selex_input = "length", selex_type = c("logistic"),
                     M50 = 14.76, maturity_input = "length", M = 2.41, binwidth = 1, CVlen = 0.1,
                     SigmaR = 0.737, SigmaF = 0.3, SigmaC = 0.1, SigmaI = 0.1, R0 = 1, Frate = 0.1,
                     Fequil = 0.25, qcoef = 1e-05, start_ages = 0, rho = 0.43, theta = 10, nseasons = 1,
                     nfleets = 1)

# lh plots
par(mfrow=c(2,2), mar=c(4,4,3,1))
plot(lh$L_a, type="l", lwd=4, xlab="Age", ylab="Length (cm)")
plot(lh$W_a, type="l", lwd=4, xlab="Age", ylab="Weight (g)")
plot(lh$Mat_l, type="l", lwd=4, xlab="Length (cm)", ylab="Proportion mature")
# plot selectivity for the first (and only) fleet (first row)
plot(lh$S_fl[1,], type="l", lwd=4, xlab="Length (cm)", ylab="Proportion selected to gear")
#######################################
# Data setup
#######################################
## identify length bins
bins <- df[,1]

## setup length frequency matrix
lf <- matrix(df[,2], nrow=1, ncol=nrow(df))

rownames(lf) <- "2018"
colnames(lf) <- bins
#######################################
# RUN LIME
#######################################
## Single year only
data_list <- list("years"=2018, "LF"=lf)

input_data <- create_inputs(lh=lh, input_data=data_list)

LFlist <- list()
LFlist[[1]] <- matrix(input_data$LF[,,1], nrow=length(input_data$years))
colnames(LFlist[[1]]) <- input_data$highs
rownames(LFlist[[1]]) <- input_data$years

LFdf <- LFreq_df(LFlist)

plot_LCfits(LF_df=LFdf)

res <- run_LIME(modpath=NULL, input=input_data, data_avail="LC", Rdet=TRUE)

## check TMB inputs
Inputs <- res$Inputs

## Report file
Report <- res$Report

## Standard error report
Sdreport <- res$Sdreport

## check convergence
hessian <- Sdreport$pdHess
gradient <- res$opt$max_gradient <= 0.001
hessian == TRUE & gradient == TRUE

## Examine the output
## SPR
spr <- Report$SPR_t

## standard error
sd_spr <- summary(Sdreport)[which(rownames(summary(Sdreport))=="SPR_t"),2]

## lower confidence limit
lcl_spr <- max(0,spr - 1.96 * sd_spr)

## upper confidence limit
ucl_spr <- spr + 1.96 * sd_spr

save <- data.frame("model"="LIME", "run"="singleyear", "spr"=spr, 
                   "lcl_spr"=lcl_spr, "ucl_spr"=ucl_spr)

F50 <- with(lh, uniroot(f=calc_ref, lower=0, upper=3, ages=ages, 
                        Mat_a=Mat_a, W_a=W_a, M=M, S_fa=Report$S_fa, ref=0.5))$root
Report$F_y/F50
(1-spr)/(1-F50)

Report$D_t


## plot length composition data and fits
plot_LCfits(Inputs=Inputs, 
            Report=Report)

abline(v=lh$linf, col="red", lwd=2, lty=2)		

## plot model output
plot_output(Inputs=Inputs, 
            Report=Report,
            Sdreport=Sdreport, 
            lh=lh,
            plot="Selex")
abline(v=lh$linf, lwd=2, lty=2, col="red")
abline(v=Report$ML_ft[length(Report$ML_ft)], lwd=2, lty=2, col="blue")
legend("topleft", legend=c("Selectivity", "Mean length in catch", "Linf"), 
       col=c("#00AA00", "blue", "red"), lty=c(1,2,2), lwd=2)

