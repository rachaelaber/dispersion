# Compute LRT p-values for the simulated data
rm(list = ls())
graphics.off()

source("code/lrt.R")


# Parameters
ww <- 8
df <- 3


# Load simulated curves
filename <- "data/processed/simulated_curves.RData"
load(filename)
rm(filename)
ncurve <- nrow(curves)

# Loop through curves and calculate p-values
pvals <- rep(NA, ncurve)

for (i in 1:ncurve) {

  iy1 <- (curve_parms$breakpoint[i] - ww + 1):curve_parms$breakpoint[i]
  iy2 <- (curve_parms$breakpoint[i] + 1):(curve_parms$breakpoint[i] + ww)

  pvals[i] <- tryCatch(lrt(
    y1 = curves[i, ][iy1],
    y2 = curves[i, ][iy2],
    s1 = curve_parms$population[i],
    s2 = curve_parms$population[i],
    i1 = 1:ww,
    i2 = (ww + 1):(2 * ww),
    df1 = df,
    df2 = df
  )$p, error = function(e) {
    return(NA)
  })

  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}

# Save p-values
filename <- "data/processed/pvals_sim_LRT.Rdata"

save(pvals, file = filename)
