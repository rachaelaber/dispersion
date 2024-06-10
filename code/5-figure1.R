rm(list = ls())

filename <- "figures/fig1.pdf"
pdf(filename)

par(mfrow = c(3, 3))

load("data/processed/simulated_curves.Rdata")
load("data/processed/processed_long_dat.Rdata") # for dates
rm(new_cases)
rm(populations)
load("data/processed/new_cases_lg_weekly.Rdata")
load("data/processed/theta_lg_pops_weekly.Rdata")
load("data/processed/pvals_sim_LRT.Rdata")

# Trim empirical incidence to same length as thetas2
series <- new_cases_lg_weekly[1, 30:(length(new_cases_lg_weekly[1,]) - 30 + 1)]
dates <- dates[30:(length(new_cases_lg_weekly[1,]) - 30 + 1)]

# Remove first observations from all
series <- series[35:length(series)]
dates <- dates[35: length(dates)]
thetas <- thetas[1, 35:dim(thetas2)[2]]

# Row 1
plot(curves[2201,], xlab = "Day", type = "l", ylab = "Incidence")
mtext("a", side = 3, line = 1, adj = 0)
plot(curves[2291,], xlab = "Day", type = "l", ylab = "Incidence")
mtext("b", side = 3, line = 1, adj = 0)
plot(dates[170:230], series[170:230], xaxt = "n", xlab = "Day", type = "l", ylab = "Incidence")

at <- seq.Date(from = min(dates[170:230]), max(dates[170:230]), by = '10 days')
labels <- seq(0, 60, by = 10)

axis.Date(1, at = at, labels = labels)
mtext("c", side = 3, line = 1, adj = 0 )


# Row 2
plot(rep(curve_parms$theta1[2201], times = length(curves[2201,])),
     xlab = "Day", ylab = expression(theta), ylim = c(0, 30), col = 4, cex = .1, type = "l")
mtext("d", side = 3, line = 1, adj = 0)
mtext("O", side = 3, line = -2)
plot(c(rep(curve_parms$theta1[2291], times = length(curves[2291,])/2), 
       rep(curve_parms$theta2[2291], times = length(curves[2291,])/2)),
     xlab = "Day", ylab = expression(theta), ylim = c(0, 30), col = 4, cex = .1, type = "l")
mtext("e", side = 3, line = 1, adj = 0)
mtext("X", side = 3, line = -2)
plot(dates[170:230], thetas[170:230], type = "l", col = 4, xaxt = "n", xlab = "Day", ylab = expression(theta))

at <- seq.Date(from = min(dates[170:230]), max(dates[170:230]), by = '10 days')
labels <- seq(0, 60, by = 10)

axis.Date(1, at = at, labels = labels)
mtext("f", side = 3, line = 1, adj = 0)



# Row 3
pops <- c(1000, 61000, 10000000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (i in 1:length(pops)) {
  current_dtheta <- dtheta[which(curve_parms$population == pops[i])]
  current_pvals <- pvals[which(curve_parms$population == pops[i])]
  
  plot(current_pvals ~ jitter(abs(current_dtheta), 1.5),
       pch = 21, col = "darkgrey", cex = 0.2,
       bg = rgb(0.4, 0.4, 0.4, 0.3),
       #main = paste("Population size", pops[i]),
       main = "",
       xlab = expression(paste(abs(theta[2] - theta[1]))),
       ylab = "p-value",
       xaxt = "n", yaxt = "n",
       ylim = c(0, 1),
       xlim = c(-0.5, 27.5)
  )
  
  mtext(letters[7:9][i], side = 3, line = 1, adj = 0)

  axis(1, seq(0, 27, 3))
  axis(2, seq(0, 1, 0.25))
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = median)
  points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = quantile, probs = c(0.25, 0.75))
  lo <- ag[, 2][, 1]
  hi <- ag[, 2][, 2]
  segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)

  text(0, 0.9, "O", cex = 1.2)
  text(27, 0.9, "X", cex = 1.2)

  

  #mtext("X", side = 3, line = -2, at = c(27, 1))
  #mtext("0", side = 3, line = -2, at = c(1, 1))
  
}

dev.off()