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

par(mfrow = c(3, 1))
plot(dates, series, type = "h", ylab = "", main = "Case counts")
plot(dates, lrt_stats[1,], type = "h", col = 2, ylab = "", main = "LRT")
plot(dates, log(thetas[1,]), type = "l", col = 4, ylab = "", main = "Log Theta")

dev.off()
