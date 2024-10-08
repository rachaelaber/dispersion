# Load data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/theta_lg_pops.Rdata")

filename <- "figures/compare.pdf"
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
plot(dates, series, type = "h", xlab = "Dates", ylab = "", main = "Case counts", cex.main = 1.9, cex.lab = 1.6, cex.axis = 1.5)
is_sig <- lrt_stats[1, ] > qchisq(0.95, df = 1) # w/o correcting for multiple testing
plot(dates, log10(thetas[1,]), type = "n", 
     xlab = "Dates", ylab = "", 
     main = expression(bold(log10(theta))), 
     cex.main = 1.9, cex.lab = 1.6, cex.axis = 1.5)
for (i in 1:(length(dates) - 1)) {
  segment_color <- ifelse(is_sig[i], "red", "blue")
    lines(dates[i:(i+1)], log10(thetas[1, i:(i+1)]), col = segment_color)
}

dev.off()
