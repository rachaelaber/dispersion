rm(list = ls())

source("code/lrt.R")


load("data/processed/simulated_curves.Rdata")
load("data/processed/nyt_weekly.Rdata") 
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/pvals_sim_LRT.Rdata")


filename <- "figures/fig1.pdf"
pdf(filename)

par(mfrow = c(3, 2))

# Trim empirical incidence to same length as thetas2
series <- cases[10, 8:(length(cases[1,]) - 8 + 1)]
dates <- dates[8:(length(cases[1,]) - 8 + 1)]

# Remove first observations from all
# series <- series[35:length(series)]
# dates <- dates[35: length(dates)]
# thetas <- thetas[1, 35:dim(thetas2)[2]]

# Row 1

plot(curves[which(curve_parms$theta1 == curve_parms$theta2 & curve_parms$breakpoint == 30 
                  & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2 & curve_parms$theta1 == 1.1),], 
     xlab = "Day", type = "l", ylab = "Incidence", cex.lab = 1.7, cex.axis = 1.7)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.7)
plot(curves[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30 
                  & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2),], 
     xlab = "Day", type = "l", ylab = "Incidence", cex.lab = 1.7, cex.axis = 1.7)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.7)



# Row 2
plot(rep(curve_parms$theta1[11188], times = length(curves[11188,])),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l", cex.lab = 1.55, cex.axis = 1.5)
mtext("c", side = 3, line = 1, adj = 0, cex = 1.7)
mtext("O", side = 3, line = -2, cex = 1.7)
plot(c(rep(curve_parms$theta1[11314], times = length(curves[11314,])/2), 
       rep(curve_parms$theta2[11314], times = length(curves[11314,])/2)),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l", cex.lab = 1.55, cex.axis = 1.5)
mtext("d", side = 3, line = 1, adj = 0, cex = 1.7)
mtext("X", side = 3, line = -2, cex = 1.7)


# Row 3
pops <- c(50000, 10000000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (i in 1:length(pops)) {
  
  current_dtheta = dtheta[which(curve_parms$population == pops[i])]
  current_pvals = pvals[which(curve_parms$population == pops[i])]
  
  plot(current_pvals ~ jitter(abs(current_dtheta), 0.01),
       pch = 21, col = "darkgrey", cex = 0.2,
       bg = rgb(0.4, 0.4, 0.4, 0.3),
       main = "",
       xlab = expression(paste(abs(theta[2] - theta[1]))),
       ylab = "p-value",
       xlim = c(0, 9),
       ylim = c(0, 1),
       xaxt = "n", yaxt = "n", cex.lab = 1.7
  )
  
  mtext(letters[5:6][i], side = 3, line = 1, adj = 0, cex = 1.7)
  
  axis(1, seq(0, 9, 3), cex.axis = 1.7)
  axis(2, seq(0, 1, 0.25), cex.axis = 1.7)
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = median)
  points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)

  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = quantile, probs = c(0.25, 0.75))
  lo <- ag[, 2][, 1]
  hi <- ag[, 2][, 2]
  segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)
  
  text(0, 0.9, "O", cex = 1.5)
  text(9, 0.9, "X", cex = 1.5)

}



dev.off()

