# Compute LRT scan statistics for large counties

source("code/lrt.R")

load("data/processed_dat.Rdata")

load("data/processed_long_dat.Rdata")

lg_pop <- which(populations$population > quantile(populations$population, 0.95))

new_cases <- new_cases[lg_pop,]

populations <- populations[lg_pop,]

lrt_stats <- matrix(NA, nrow = length(lg_pop), ncol = 1124)

for (j in 1:nrow(new_cases)){
  
  print(j)
  
  series = new_cases[j,]
  
  lrt_stat <- c()
  
  for (i in 30:(length(series) - 30 + 1)){
    lrt_stat = c(lrt_stat, tryCatch(lrt(y1 = series[(i - 29):i], y2 = series[(i + 1):(i + 30)], 
                                        s1 = populations$population[j], 
                                        s2 = populations$population[j],
                                        i1 = (i - 29):i,
                                        i2 = (i + 1):(i + 30), 
                                        df1 = 3, 
                                        df2 = 3)$lambda, error = function(e) return(NA)))
  }
  
  lrt_stats[j,] <- lrt_stat
}

# Save LRT statistic array

filename <- "data/lrt_lg_pops.Rdata"

save(lrt_stats, file = filename)

# Produce figure for one county - use first (of large populations) full time series 

this_series <- new_cases[1, ]

dates <- dates[30:(length(this_series) - 30 + 1)]

filename <- "figures/LRT_scan_figure.pdf"

pdf(filename, width = 6, height = 6)

plot(lrt_stats[1, ] ~ dates, type = "l", xlab = "Date", ylab = "LRT Statistic")

plot(this_series[30:(length(this_series) - 30 + 1)] ~ dates, col = "red", type = "l")

abline(h = quantile(lrt_stats[1, ], probs = 0.95, na.rm =TRUE), col = "black")

abline(h = qchisq(0.95, 1), col = "blue")

dev.off()