# Loop through simulated curves and calculate theta0 estimates for those curves where theta remains the same
rm(list = ls())
graphics.off()

source("code/lrt.R")

# Load simulated curves
filename <- "data/processed/simulated_curves.RData"
load(filename)
rm(filename)

inds <- which(curve_parms$theta1 == curve_parms$theta2) # indices where true theta remains the same
curves <- curves[inds, ] # subset curves by those indices
ncurve <- nrow(curves)
curve_parms <- curve_parms[inds, ]


phi0s <- rep(NA, length(inds))

for (i in 1:nrow(curves)) {
  phi0s[i] <- tryCatch(lrt(
    y1 = curves[i, ][1:30],
    y2 = curves[i, ][31:60],
    s1 = curve_parms$population[i],
    s2 = curve_parms$population[i],
    i1 = 1:30,
    i2 = 31:60,
    df1 = 3,
    df2 = 3
  )$phi0, error = function(e) return(NA))
  
  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}

# Plot against true theta0 values
theta0s <- 1/phi0s
true_theta <- curve_parms$theta1

plot(log10(true_theta), log10(theta0s))
abline(0, 1, col = "red")