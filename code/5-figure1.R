rm(list = ls())

source("code/lrt.R")


load("data/processed/simulated_curves.Rdata")
load("data/processed/nyt_weekly.Rdata") 
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/pvals_sim_LRT.Rdata")


filename <- "figures/fig1.pdf"
pdf(filename)

par(mfrow = c(3, 3))

# Trim empirical incidence to same length as thetas2
series <- cases[10, 8:(length(cases[1,]) - 8 + 1)]
dates <- dates[8:(length(cases[1,]) - 8 + 1)]

# Remove first observations from all
# series <- series[35:length(series)]
# dates <- dates[35: length(dates)]
# thetas <- thetas[1, 35:dim(thetas2)[2]]

# Row 1

plot(curves[which(curve_parms$theta1 == curve_parms$theta2 & curve_parms$breakpoint == 30 
                  & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2 & curve_parms$theta1 == 1e+01),], 
     xlab = "Day", type = "l", ylab = "Incidence")
mtext("a", side = 3, line = 1, adj = 0)
plot(curves[which(curve_parms$theta1 == 1e-01 & curve_parms$theta2 == 1e+02 & curve_parms$breakpoint == 30 
                  & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2),], 
     xlab = "Day", type = "l", ylab = "Incidence")
mtext("b", side = 3, line = 1, adj = 0)
plot(dates[1:61], series[1:61], xaxt = "n", xlab = "Day", type = "l", ylab = "Incidence")

at <- seq.Date(from = min(dates[1:61]), to = max(dates[1:61]), by = "week")
labels <- seq(0, 60)

axis.Date(1, at = at, labels = labels)
mtext("c", side = 3, line = 1, adj = 0 )


# Row 2
plot(rep(curve_parms$theta1[1707], times = length(curves[1707,])),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l")
mtext("d", side = 3, line = 1, adj = 0)
mtext("O", side = 3, line = -2)
plot(c(rep(curve_parms$theta1[1711], times = length(curves[1711,])/2), 
       rep(curve_parms$theta2[1711], times = length(curves[1711,])/2)),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l")
mtext("e", side = 3, line = 1, adj = 0)
mtext("X", side = 3, line = -2)
plot(dates[1:61], thetas[10, 1:61], type = "l", col = 4, xaxt = "n", xlab = "Day", ylab = expression(theta))

at <- seq.Date(from = min(dates[1:61]), to = max(dates[1:61]), by = "week")
labels <- seq(0, 60)

axis.Date(1, at = at, labels = labels)
mtext("f", side = 3, line = 1, adj = 0)


# Row 3
pops <- c(50000, 10000000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (i in 1:length(pops)) {
  
  current_dtheta = dtheta[which(curve_parms$population == pops[i])]
  current_pvals = pvals[which(curve_parms$population == pops[i])]
  
  plot(current_pvals ~ jitter(abs(current_dtheta), 1.5),
       pch = 21, col = "darkgrey", cex = 0.2,
       bg = rgb(0.4, 0.4, 0.4, 0.3),
       main = "",
       xlab = expression(paste(abs(theta[2] - theta[1]))),
       ylab = "p-value",
       xlim = c(0, 100),
       ylim = c(0, 1),
       xaxt = "n", yaxt = "n",
  )
  
  mtext(letters[7:8][i], side = 3, line = 1, adj = 0)
  
  axis(1, seq(0, 6, 3))
  axis(2, seq(0, 1, 0.25))
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = median)
  points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)
  
  ag <- aggregate(current_pvals ~ abs(current_dtheta), FUN = quantile, probs = c(0.25, 0.75))
  lo <- ag[, 2][, 1]
  hi <- ag[, 2][, 2]
  segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)
  
  text(0, 0.9, "O", cex = 1.2)

}

#3,3
inds <- which(curve_parms$theta1 == curve_parms$theta2) # indices where true theta remains the same
curves <- curves[inds, ] # subset curves by those indices
ncurve <- nrow(curves)
curve_parms <- curve_parms[inds, ]


phi0s <- rep(NA, length(inds))

for (i in 1:nrow(curves)) {
  phi0s[i] <- tryCatch(lrt(
    y1 = curves[i, ][(curve_parms$breakpoint[i]-7):curve_parms$breakpoint[i]],
    y2 = curves[i, ][(curve_parms$breakpoint[i]+1):(curve_parms$breakpoint[i] + 8)],
    s1 = curve_parms$population[i],
    s2 = curve_parms$population[i],
    i1 = 1:8,
    i2 = 9:16,
    df1 = 3,
    df2 = 3
  )$phi0, error = function(e) return(NA))
  
  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}

theta0s <- 1/phi0s
true_theta <- curve_parms$theta1

plot(log10(true_theta), log10(theta0s))
abline(0, 1, col = "red")

dev.off()

