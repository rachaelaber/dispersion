# Test validity and power of the proposed LRT on simulated data

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
filename <- "data/pvals_sim_LRT.Rdata"
save(pvals, file = filename)

# Visualize p-values
par(mfrow = c(1,2))
hist(pvals[which(curve_parms$theta1==curve_parms$theta2)], col=1)
hist(pvals[which(curve_parms$theta1!=curve_parms$theta2)], col=2)
