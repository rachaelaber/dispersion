# Test validity and power of the proposed Wald test on simulated data

source("code/W.R")

# Load simulated curves
filename <- "data/simulated_curves.RData"
load(filename)
rm(filename)
ncurve <- nrow(curves)

# Loop through curves and calculate p values
pvals <- rep(NA, ncurve)

for (i in 1:ncurve) {
  pvals[i] <- W(curves[i, ], population_size = curve_parms$population[i], breakpoint = curve_parms$breakpoint[i], deg_free = 3, 
           fn = my_spl_fit, verbose = FALSE)
}

# Save p-values
filename <- "data/pvals_sim_Wald.Rdata"
save(pvals, file = filename)
