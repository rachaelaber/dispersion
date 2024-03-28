# Using simulated data, visualize relationship between p-values, dtheta, 
# and population size

load("data/pvals_sim_LRT.Rdata")

load("data/simulated_curves.Rdata")

pops <- c(314000, 1570000, 3140000)

filename <- "figures/pvals_sim_LRT.png"

png(filename)

par(mfrow = c(1, 3))

dtheta <- curve_parms$theta2 - curve_parms$theta1

for (pop in pops) {
  current_dtheta <- dtheta[which(curve_parms$population == pop)]
  current_pvals <- pvals[which(curve_parms$population == pop)]
  
  plot(current_pvals ~ jitter(abs(current_dtheta), 1.5),
       pch = 21, col = NA, cex = 0.2,
       bg = rgb(0.4, 0.4, 0.4, 0.3),
       xlab = expression(paste(abs(theta[2] - theta[1]))),
       ylab = "p-value",
       xaxt = "n", yaxt = "n",
       ylim = c(0, 1),
       xlim = c(-0.5, 27.5)
  )
  
  axis(1, seq(0, 27, 3))
  axis(2, seq(0, 1, 0.25))
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = mean)
  points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = quantile, probs = c(0.25, 0.75))
  lo <- ag[, 2][, 1]
  hi <- ag[, 2][, 2]
  segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)
  
}

dev.off()
