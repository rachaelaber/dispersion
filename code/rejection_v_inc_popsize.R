# Using simulated data, visualize relationship between p-values
# and population size

load("data/pvals_sim_LRT.Rdata")
load("data/simulated_curves.Rdata")

filename <- "figures/sim_pval_v_pop.pdf"
pdf(filename, width = 6, height = 6)

par(cex = 1.5)
par(pin = c(3, 3))

plot(pvals ~ jitter(curve_parms$population, 1.5),
     pch = 21, col = NA, cex = 0.2,
     bg = rgb(0.4, 0.4, 0.4, 0.3),
     xlab = "Population Size",
     ylab = "p-value",
     xaxt = "n", yaxt = "n",
     ylim = c(0, 1),
)

axis(1, seq(314000, 3140000, length = 10))
axis(2, seq(0, 1, 0.25))

ag <- aggregate(pvals ~ curve_parms$population, FUN = mean)
points(ag[, 1], ag[, 2], pch = 19, col = 2, cex = 1)

ag <- aggregate(pvals ~ curve_parms$population, FUN = quantile, probs = c(0.25, 0.75))
lo <- ag[, 2][, 1]
hi <- ag[, 2][, 2]
segments(ag[, 1], lo, ag[, 1], hi, col = 2, lwd = 3)

dev.off()