# Test validity and power of the proposed Wald test on simulated data

rm(list = ls())
graphics.off()

library(gridExtra)
source("code/W.R")

# Load simulated curves
filename <- "data/simulated_curves.RData"
load(filename)
rm(filename)
ncurve <- nrow(curves)

# Loop through curves and calculate p values
pvals <- rep(NA, ncurve)
stats <- rep(NA, ncurve)

for (i in 1:ncurve) {
  pvals[i] <- W(curves[i, ], population_size = curve_parms$population[i],
                breakpoint = curve_parms$breakpoint[i], deg_free = 3,
                fn = my_spl_fit, verbose = FALSE)
  
  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}

# Save p-values
filename <- "data/pvals_sim_Wald.Rdata"
save(pvals, file = filename)

# Tabulate Type I error rate and Power

p_vals.0 <- pvals[which(curve_parms$theta2 - curve_parms$theta1 == 0)]
pow.0 <- mean(p_vals.0 < 0.05)

p_vals.3 <- pvals[which(curve_parms$theta2 - curve_parms$theta1 == 3)]
pow.3 <- mean(p_vals.3 < 0.05)

p_vals.9 <- pvals[which(curve_parms$theta2 - curve_parms$theta1 == 9)]
pow.9 <- mean(p_vals.9 < 0.05)


X <- data.frame("Type I Error Rate" =  round(pow.0, 2),
                "Power at 3" = round(pow.3, 2),
                "Power at 9" = round(pow.9, 2))

filename <- "figures/pvals_sim_Wald_table.pdf"

pdf(filename, width = 6, height = 6)

grid.table(X)

dev.off()

# Visualize p-values
dtheta <- curve_parms$theta2 - curve_parms$theta1

filename <- "figures/pvals_sim_Wald.pdf"

pdf(filename, width = 6, height = 6)

par(cex = 1.5)
par(pin = c(3, 3))

plot(pvals ~ jitter(abs(dtheta), 1.5),
  pch = 21, col = NA, cex = 0.2,
  bg = rgb(0.4, 0.4, 0.4, 0.3),
  xlab = expression(paste(abs(theta[2] - theta[1]))),
  ylab = "p-value",
  xaxt = "n", yaxt = "n",
  ylim = c(0, 1),
  xlim = c(-0.5, 9.5))
axis(1, seq(0, 9, 3))
axis(2, seq(0, 1, 0.25))

ag <- aggregate(pvals ~ abs(dtheta), FUN = mean)
points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)

ag <- aggregate(pvals ~ abs(dtheta), FUN = quantile, probs = c(0.25, 0.75))
lo <- ag[, 2][, 1]
hi <- ag[, 2][, 2]
segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)

dev.off()
