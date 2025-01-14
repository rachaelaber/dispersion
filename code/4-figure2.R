# Load data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/theta_lg_pops.Rdata")

filename <- "figures/fig2.pdf"
pdf(filename, width = 6, height = 6)

# Presumed reporting rate
reporting_rate <- 0.10

# Trim to same length as outputs
series <- cases[1, 8:(length(cases[1,]) - 8)]
dates <- dates[8:(length(cases[1,]) - 8)]

# Plot

par(mfrow = c(2, 1))
plot(dates, series, type = "h", xlab = "Dates", ylab = "", main = "Case counts", cex.main = 1.3, cex.lab = 1, cex.axis = 1)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.3)


is_sig <- lrt_stats[1, ] > qchisq(0.9996732, df = 1) 
plot(dates, log10(thetas[1,]), type = "n", 
     xlab = "Dates", ylab = "", 
     main = expression(bold(log10(theta))), 
     cex.main = 1.3, cex.lab = 1, cex.axis = 1, ylim = c(0, 5))
for (i in 1:(length(dates) - 1)) {
  segment_color <- ifelse(is_sig[i], "red", "blue")
    lines(dates[i:(i+1)], log10(thetas[1, i:(i+1)]), col = segment_color, lwd = 2)
}

lines(dates, log10(c(NA, series[-length(series)])) - log10(reporting_rate), lwd = 2)

# legend("left", legend = c(expression(chi^2 >= 12.92225), expression(chi^2 < 12.92225)), 
#        lty = 1, col = c("red", "blue"), cex = 0.6, bty = "n")
mtext("b", side = 3, line = 1, adj = 0, cex = 1.3)

dev.off()
