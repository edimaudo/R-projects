# LIME installation
#install.packages("devtools", repos='http://cran.us.r-project.org')
#devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
#devtools::install_github("merrillrudd/LIME", dependencies=TRUE)

# clear environment
rm(list = ls()) 
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','LIME')
#=============
# load packages
#=============
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data
df <- read_excel(file.choose())

# Length class update
# convert length from mm to cm
update_mm_to_cm <- function(x) {
  return (x/10)
}

df$`Lengthclass (mm)` <- sapply(df$`Lengthclass (mm)`, update_mm_to_cm)

# Perform analysis
# simulation and estimation features require the biological inputs
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

# simulated population
true <- generate_data(modpath = NULL, itervec = 1, Fdynamics = c("Endogenous"),
                     Rdynamics = "Constant", lh = lh, Nyears = 20, Nyears_comp = c(20), 
                     comp_sample = rep(200,20), init_depl = 0.7, seed = 123, fleet_proportions = 1)

par(mfrow = c(3, 2))
plot(true$F_ft[1, ], type = "l", lwd = 4, xlab = "Year", ylab = "Fishing mortality",
     ylim = c(0, max(true$F_ft) * 1.1), xaxs = "i", yaxs = "i", cex.axis = 1.5,
     cex.lab = 1.5)
plot(true$R_t, type = "l", lwd = 4, xlab = "Year", ylab = "Recruitment", ylim = c(0,
                                                                                  max(true$R_t) * 1.1), xaxs = "i", yaxs = "i", cex.axis = 1.5, cex.lab = 1.5)
plot(true$SPR_t, type = "l", lwd = 4, xlab = "Year", ylab = "SPR", ylim = c(0,
                                                                            1), xaxs = "i", yaxs = "i", cex.axis = 1.5, cex.lab = 1.5)
plot(true$D_t, type = "l", lwd = 4, xlab = "Year", ylab = "Relative spawning biomass",
     ylim = c(0, max(true$D_t) * 1.1), xaxs = "i", yaxs = "i", cex.axis = 1.5,
     cex.lab = 1.5)
plot(true$Cw_ft[1, ], type = "l", lwd = 4, xlab = "Year", ylab = "Catch", ylim = c(0,
                                                                                   max(true$Cw_ft) * 1.1), xaxs = "i", yaxs = "i", cex.axis = 1.5, cex.lab = 1.5)
plot(true$I_ft[1, ], type = "l", lwd = 4, xlab = "Year", ylab = "Abundance index",
     ylim = c(0, max(true$I_ft) * 1.1), xaxs = "i", yaxs = "i", cex.axis = 1.5,
     cex.lab = 1.5)

