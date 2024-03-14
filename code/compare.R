# Note that the first observations are omitted in these plots

load("data/processed_long_dat.Rdata")
rm(new_cases)
rm(populations)

load("data/new_cases_lg.Rdata")

load("data/lrt_lg_pops.Rdata")

load("data/theta_lg_pops.Rdata")

filename <- "figures/compare.png"

png(filename)

# Trim to same length as outputs
series <- new_cases_lg[1, 30:(length(new_cases_lg[1,]) - 30 + 1)]

dates <- dates[30:(length(new_cases_lg[1,]) - 30 + 1)]

# Remove first observations from all

series <- series[35:length(series)]

dates <- dates[35: length(dates)]

lrt_stats <- lrt_stats[1, 35:dim(lrt_stats)[2]]

thetas <- thetas[1, 35:dim(thetas)[2]]

# Plot

par(mfrow = c(3, 1))
  
plot(dates, series, type = "h", ylab = "", main = "Incidence")
  
plot(dates, lrt_stats, type = "h", col = 2, ylab = "", main = "LRT")
  
plot(dates, log(thetas), type = "l", col = 4, ylab = "", main = "Log Theta")

dev.off()
