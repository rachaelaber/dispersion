# Test validity and power of the proposed LRT
# using simulated data

source("code/lrt.R")

# Load simulated curves
filename <- "data/simulated_curves.RData"
load(filename)
rm(filename)
ncurve <- nrow(curves)

# Loop through curves and calculate p values
pvals <- rep(NA, ncurve)

for (i in 1:ncurve) {
  pvals[i] <- lrt(y1 = curves[i, ][1:30], y2 = curves[i, ][31:60], s1 = curve_parms$population[i], 
                  s2 = curve_parms$population[i], i1 = 1:30, i2 = 31:60, df1 = 3, df2 = 3)$p
}

# Save p-values
filename <- "data/p_values_simulated_curves_LRT.Rdata"
save(pvals, file = filename)
