# LIME installation
#install.packages("devtools", repos='http://cran.us.r-project.org')
#devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
#devtools::install_github("merrillrudd/LIME", dependencies=TRUE)

#######################################
# clear environment
#######################################
rm(list = ls()) 

#######################################
# Packages
#######################################
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','LIME','LBSPR',
              'TMB','TMBhelper')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#######################################
# Load data
#######################################
df <- read_excel(file.choose())

#######################################
# Data update
#######################################
# Length update
# convert length from mm to cm
update_mm_to_cm <- function(x) {
  return (x/10)
}
df[,c(1:13)] <- lapply(df[,c(1:13)], update_mm_to_cm)
# Add year
Year <- c(1:12)
df <- cbind(Year, df)
# update column names
colnames(df) <- c("Year","0.5","1.5","2.5","3.5","4.5","5.5",
                  "6.5","7.5","8.5","9.5","10.5","11.5","12")
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
# par(mfrow=c(2,2), mar=c(4,4,3,1))
# plot(lh$L_a, type="l", lwd=4, xlab="Age", ylab="Length (cm)")
# plot(lh$W_a, type="l", lwd=4, xlab="Age", ylab="Weight (g)")
# plot(lh$Mat_l, type="l", lwd=4, xlab="Length (cm)", ylab="Proportion mature")
# # plot selectivity for the first (and only) fleet (first row)
# plot(lh$S_fl[1,], type="l", lwd=4, xlab="Length (cm)", ylab="Proportion selected to gear")

#######################################
## Other data type input options
#######################################
h <- create_lh_list(vbk = 0.21, linf = 65, t0 = -0.01, lwa = 0.0245, lwb = 2.79,
                    S50 = c(20), S95 = c(26), selex_input = "length", selex_type = c("logistic"),
                    M50 = 34, maturity_input = "length", M = 0.27, binwidth = 1, CVlen = 0.1,
                    SigmaR = 0.737, SigmaF = 0.2, SigmaC = 0.1, SigmaI = 0.1, R0 = 1, Frate = 0.1,
                    Fequil = 0.25, qcoef = 1e-05, start_ages = 0, rho = 0.43, theta = 10, nseasons = 1,
                    nfleets = 1)

true <- generate_data(modpath = NULL, itervec = 1, Fdynamics = c("Endogenous"),
                     Rdynamics = "Constant", lh = h, Nyears = 12, Nyears_comp = c(12), 
                     comp_sample = rep(12,12), init_depl = 0.7, seed = 123, fleet_proportions = 1)
#using simulation data
data_all <- list(years = 1:true$Nyears, LF = true$LF, I_ft = true$I_ft, C_ft = true$Cw_ft,
                 neff_ft = true$obs_per_year)
inputs_all <- create_inputs(lh=h, input_data=data_all)

## Using user data
#months <- 1:12
#LenFreqMAT <- df[,2:14] #(as.matrix(df[,2:14]))
#rownames(LenFreqMAT) <- months
#mids <- c("0.5","1.5","2.5","3.5","4.5","5.5",
#"6.5","7.5","8.5","9.5","10.5","11.5","12")
#colnames(LenFreqMAT) <- mids
#data_all <- list(years = months, LF=df[,2:14],
                #C_ft=df$Catch,
#                neff_ft = true$obs_per_year)
#inputs_all <- create_inputs(lh=lh, input_data=data_all)

#######################################
## Data-rich test
#######################################
rich <- run_LIME(modpath=NULL,
                 input=inputs_all,
                 data_avail="LC",  #LC, Catch_LC, Index_LC,Index_Catch_LC
                 C_type=2) #0, 1,2

## check TMB inputs
Inputs <- rich$Inputs

## Report file
Report <- rich$Report

## Standard error report
Sdreport <- rich$Sdreport

## check convergence
hessian <- Sdreport$pdHess
gradient <- rich$opt$max_gradient <= 0.001
hessian == TRUE & gradient == TRUE


## plot length composition data and fits
plot_LCfits(Inputs=Inputs,
            Report=Report)

plot_LCfits(Inputs=Inputs,
            Report=Report,
            plot_fit=FALSE)


## plot model output
plot_output(Inputs=Inputs,
            Report=Report,
            Sdreport=Sdreport,
            lh=lh,
            True=true,
            plot=c("Fish","Rec","SPR","ML","SB","Selex"),
            set_ylim=list("SPR" = c(0,1)))

plot(true$BBmsy, ylim=c(0,4))
lines(rich$Derived$BBmsy)


#######################################
## Length-data only
#######################################

lc_only <- run_LIME(modpath=NULL, 
                    input=inputs_all, 
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

## hessian not positive definite -- the following line helps diagnose which 
## parameters can't be estimated 
check <- TMBhelper::Check_Identifiable(lc_only$obj)

## issues estimating F - try a more narrow penalty on F
inputs_LC_new <- inputs_all
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

## plot model output
plot_output(Inputs=Inputs, 
            Report=Report,
            Sdreport=Sdreport, 
            lh=lh,
            True=true, 
            LBSPR=lbspr,
            plot=c("Fish","Rec","SPR","ML","SB","Selex"), 
            set_ylim=list("Fish" =c(0,1), "SPR" = c(0,1), "SB"=c(0,2)))	



#######################################
## Catch + length data
#######################################
# catch_lc <- run_LIME(modpath=NULL, 
#                      input=inputs_all,
#                      data_avail="Catch_LC",
#                      C_type=2)
# 
# ## check TMB inputs
# Inputs <- catch_lc$Inputs
# 
# ## Report file
# Report <- catch_lc$Report
# 
# ## Standard error report
# Sdreport <- catch_lc$Sdreport
# 
# ## check convergence
# hessian <- Sdreport$pdHess
# gradient <- catch_lc$opt$max_gradient <= 0.001
# hessian == TRUE & gradient == TRUE
# 
# 
# ## plot length composition data
# plot_LCfits(Inputs=Inputs, 
#             Report=Report)		
# 
# ## plot model output
# plot_output(Inputs=Inputs, 
#             Report=Report,
#             Sdreport=Sdreport, 
#             lh=lh,
#             True=true, 
#             plot=c("Fish","Rec","SPR","ML","SB","Selex"), 
#             set_ylim=list("SPR" = c(0,1), "SB"=c(0,2)))		
# 
# 
# ### remove some years of catch
# LF_list2 <- lapply(1:lh$nfleets, function(x){
#   out <- LF_list[[x]]
#   out[1:15,] <- 0
#   return(out)
# }) ##list with 1 element per fleet, and each element is a matrix with rows = years, 
#    ##columns = upper length bins
# LF_df2 <- LFreq_df(LF_list2)
# 
# 
# data_rm <- list("years"=df$Year, "LF"=LF_df2, "C_ft"=df$Catch)
# inputs_rm <- create_inputs(lh=lh, input_data=data_rm)
# 
# catch_lc2 <- run_LIME(modpath=NULL, 
#                       input=inputs_rm,
#                       data_avail="Catch_LC",
#                       C_type=2,
#                       est_rdev_t = c(rep(0,5),rep(1,15)))
# 
# ## check TMB inputs
# Inputs <- catch_lc2$Inputs
# 
# ## Report file
# Report <- catch_lc2$Report
# 
# ## Standard error report
# Sdreport <- catch_lc2$Sdreport
# 
# ## check convergence
# hessian <- Sdreport$pdHess
# gradient <- catch_lc2$opt$max_gradient <= 0.001
# hessian == TRUE & gradient == TRUE
# 
# 
# ## plot length composition data
# plot_LCfits(Inputs=Inputs, 
#             Report=Report)		
# 
# ## plot model output
# plot_output(Inputs=Inputs, 
#             Report=Report,
#             Sdreport=Sdreport, 
#             lh=lh,
#             True=true, 
#             plot=c("Fish","Rec","SPR","ML","SB","Selex"), 
#             set_ylim=list("SPR" = c(0,1), "SB"=c(0,2)))
# 
