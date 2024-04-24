# Compute LRT p-values for the simulated data
rm(list = ls())
graphics.off()

source("code/lrt.R")

# Load simulated curves
filename <- "data/processed/simulated_curves.RData"
load(filename)
rm(filename)
ncurve <- nrow(curves)

# Loop through curves and calculate p-values
pvals <- rep(NA, ncurve)

for (i in 1:ncurve) {
  pvals[i] <- tryCatch(lrt(
    y1 = curves[i, ][1:30],
    y2 = curves[i, ][31:60],
    s1 = curve_parms$population[i],
    s2 = curve_parms$population[i],
    i1 = 1:30,
    i2 = 31:60,
    df1 = 3,
    df2 = 3
  )$p, error = function(e) return(NA))

  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}

# Save p-values
filename <- "data/processed/pvals_sim_LRT.Rdata"

save(pvals, file = filename)
