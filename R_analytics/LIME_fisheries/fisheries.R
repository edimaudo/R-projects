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
df <- read_csv(file.choose())
df<- sapply(df, as.integer)
df <- as.data.frame(df)
#######################################
# LH information
#######################################
# 1 fleets
lh <- create_lh_list(vbk = 0.94, linf = 57.84, t0 = -0.01, lwa = 0.023, lwb = 2.81, 
                     S50 = c(20.84), S95 = c(30), selex_input = "length", 
                     selex_type = c("logistic"), M50 = 30, maturity_input = "length", 
                     M = 1.033, binwidth = 1, CVlen = 0.1, SigmaR = 0.737, SigmaF = 0.2, 
                     SigmaC = 0.1, SigmaI = 0.1, R0 = 1, Frate = 0.1, Fequil = 0.25, 
                     qcoef = 1e-05, start_ages = 0, rho = 0.43, theta = 10, nseasons = 1,
                                   nfleets = 1)

# lh plots
par(mfrow=c(2,2), mar=c(4,4,3,1))
plot(lh$L_a, type="l", lwd=4, xlab="Age", ylab="Length (cm)")
plot(lh$W_a, type="l", lwd=4, xlab="Age", ylab="Weight (g)")
plot(lh$Mat_l, type="l", lwd=4, xlab="Length (cm)", ylab="Proportion mature")
# plot selectivity for the first (and only) fleet (first row)
plot(lh$S_fl[1,], type="l", lwd=4, xlab="Length (cm)", ylab="Proportion selected to gear")

#######################################
## Data prep
#######################################
df$Fleet<- rowSums(df[ , c(2:13)], na.rm=TRUE)
df <- df[,c(1,14)]
df<- sapply(df, as.integer)
df <- as.data.frame(df)

## identify length bins
bins <- df[,1]

## setup length frequency matrix
lf <- matrix(df[,2], nrow=1, ncol=nrow(df))
rownames(lf) <- "2018"
colnames(lf) <- bins
#######################################
## Run LIME
#######################################
## Single year only
data_list <- list("years"=2018, "LF"=lf)
input_data <- create_inputs(lh=lh, input_data=data_list)
inputs_LC <- create_inputs(lh=lh, input_data=data_list)
inputs_all <- create_inputs(lh=lh, input_data=data_list)

LFlist <- list()
LFlist[[1]] <- matrix(input_data$LF[,,1], nrow=length(input_data$years))
colnames(LFlist[[1]]) <- df$Lengthclass
rownames(LFlist[[1]]) <- input_data$years

LFdf <- LFreq_df(LFlist)
plot_LCfits(LF_df=LFdf)

#######################################
## Length-data only
#######################################
lc_only <- run_LIME(modpath=NULL, 
                   input=inputs_LC,
                   data_avail="LC")

## check TMB inputs
Inputs <- lc_only$Inputs

## Report file
Report <- lc_only$Report

## Standard error report
Sdreport <- lc_only$Sdreport

## check convergence
hessian <- Sdreport$pdHess
gradient <- lc_only$opt$max_gradient <= 0.001
hessian == TRUE & gradient == TRUE

## hessian not positive definite -- the following line helps 
# diagnose which parameters can't be estimated 
check <- TMBhelper::Check_Identifiable(lc_only$obj)

## issues estimating F - try a more narrow penalty on F
inputs_LC_new <- inputs_LC
inputs_LC_new$SigmaF <- 0.1

lc_only2 <- run_LIME(modpath=NULL, 
                     input=inputs_LC_new,
                     data_avail="LC")

## check TMB inputs
Inputs <- lc_only2$Inputs

## Report file
Report <- lc_only2$Report

## Standard error report
Sdreport <- lc_only2$Sdreport

## check convergence
hessian <- Sdreport$pdHess
gradient <- lc_only$opt$max_gradient <= 0.001
hessian == TRUE & gradient == TRUE

## LBSPR
LB_pars <- new("LB_pars")
LB_pars@MK <- inputs_all$M/inputs_all$vbk
LB_pars@Linf <- inputs_all$linf
LB_pars@L50 <- inputs_all$ML50
LB_pars@L95 <- inputs_all$ML95
LB_pars@Walpha <- inputs_all$lwa
LB_pars@Wbeta <- inputs_all$lwb
LB_pars@R0 <- inputs_all$R0
LB_pars@Steepness <- ifelse(inputs_all$h==1, 0.99, inputs_all$h)
LB_pars@BinWidth <- inputs_all$binwidth

LB_lengths <- new("LB_lengths")
LB_lengths@LMids <- inputs_all$mids
LB_lengths@LData <- t(matrix(inputs_all$LF, ncol=length(inputs_all$mids)))
LB_lengths@Years <- as.numeric(rownames(inputs_all$LF))
LB_lengths@NYears <- ncol(LB_lengths@LData)

lbspr <- LBSPRfit(LB_pars=LB_pars, LB_lengths=LB_lengths)

## plot length composition data
plot_LCfits(Inputs=Inputs, 
            Report=Report,
            LBSPR=lbspr)		

## plot model output -- not working due to InputMat[, ]
# plot_output(Inputs=Inputs, 
#             Report=Report,
#             Sdreport=Sdreport, 
#             lh=lh,
#             True=true, 
#             LBSPR=lbspr,
#             plot=c("Fish","Rec","SPR","ML","SB","Selex"), 
#             set_ylim=list("Fish" =c(0,1), "SPR" = c(0,1), "SB"=c(0,2)))	

# This works
plot_output(Inputs=Inputs, 
            Report=Report,
            Sdreport=Sdreport, 
            lh=lh,
            True=true, 
            LBSPR=lbspr,
            plot=c("Selex"), 
            set_ylim=list("Fish" =c(0,1), "SPR" = c(0,1), "SB"=c(0,2)))	