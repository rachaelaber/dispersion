# Load data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/theta_lg_pops.Rdata")

filename <- "figures/fig2.pdf"
pdf(filename, width = 6, height = 6)

# Trim to same length as outputs
series <- cases[1, 8:(length(cases[1,]) - 8 + 1)]
dates <- dates[8:(length(cases[1,]) - 8 + 1)]

# Remove first observations from all
#series <- series[35:length(series)]
#dates <- dates[35: length(dates)]
#lrt_stats <- lrt_stats[1, 35:dim(lrt_stats)[2]]
#thetas <- thetas[1, 35:dim(thetas)[2]]

# Plot

par(mfrow = c(2, 1))
plot(dates, series, type = "h", xlab = "Dates", ylab = "", main = "Case counts", cex.main = 1.3, cex.lab = 1, cex.axis = 1)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.3)


is_sig <- lrt_stats[1, ] > qchisq(0.95, df = 1) # w/o correcting for multiple testing
plot(dates, log10(thetas[1,]), type = "n", 
     xlab = "Dates", ylab = "", 
     main = expression(bold(log10(theta))), 
     cex.main = 1.3, cex.lab = 1, cex.axis = 1)
for (i in 1:(length(dates) - 1)) {
  segment_color <- ifelse(is_sig[i], "red", "blue")
    lines(dates[i:(i+1)], log10(thetas[1, i:(i+1)]), col = segment_color)
}
legend("bottomleft", legend = c(expression(chi^2 >= 3.841459), expression(chi^2 < 3.841459)), 
       lty = 1, col = c("red", "blue"), cex = 0.6, bty = "n")
mtext("b", side = 3, line = 1, adj = 0, cex = 1.3)

dev.off()
