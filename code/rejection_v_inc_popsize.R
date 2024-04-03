filename <- "figures/fig1.png"
png(filename)

par(mfrow = c(3, 3))

load("data/simulated_curves.Rdata")
load("data/processed_long_dat.Rdata")
rm(new_cases)
rm(populations)
load("data/new_cases_lg.Rdata")
load("data/theta_lg_pops.Rdata")
load("data/pvals_sim_LRT.Rdata")


# Trim empirical incidence to same length as thetas
series <- new_cases_lg[1, 30:(length(new_cases_lg[1,]) - 30 + 1)]
dates <- dates[30:(length(new_cases_lg[1,]) - 30 + 1)]

# Remove first observations from all
series <- series[35:length(series)]
dates <- dates[35: length(dates)]
thetas <- thetas[1, 35:dim(thetas)[2]]

# Row 1
plot(curves[9701,], xlab = "Day of Epidemic", type = "l", ylab = "Incidence")
plot(curves[9710,], xlab = "Day of Epidemic", type = "l", ylab = "Incidence")
plot(dates, series, xlab = "Date", type = "h", ylab = "Incidence")


# Row 2
plot(rep(curve_parms$theta1[9701], times = length(curves[9701,])),
     xlab = "Day of Epidemic", ylab = expression(theta))
plot(c(rep(curve_parms$theta1[9710], times = length(curves[9710,])/2), 
       rep(curve_parms$theta2[9710], times = length(curves[9710,])/2)),
     xlab = "Day of Epidemic", ylab = expression(theta))
plot(dates, log(thetas), type = "l", col = 4, xlab = "Date", ylab = expression(log(theta)))

# Row 3
pops <- c(314000, 1570000, 3140000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (pop in pops) {
  current_dtheta <- dtheta[which(curve_parms$population == pop)]
  current_pvals <- pvals[which(curve_parms$population == pop)]
  
  plot(current_pvals ~ jitter(abs(current_dtheta), 1.5),
       pch = 21, col = NA, cex = 0.2,
       bg = rgb(0.4, 0.4, 0.4, 0.3),
       main = paste("Population size", pop),
       xlab = expression(paste(abs(theta[2] - theta[1]))),
       ylab = "p-value",
       xaxt = "n", yaxt = "n",
       ylim = c(0, 1),
       xlim = c(-0.5, 27.5)
  )
  
  axis(1, seq(0, 27, 3))
  axis(2, seq(0, 1, 0.25))
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = median)
  points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = quantile, probs = c(0.25, 0.75))
  lo <- ag[, 2][, 1]
  hi <- ag[, 2][, 2]
  segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)
  
}

dev.off()