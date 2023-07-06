# Compute LRT scan statistics for all counties

source("code/lrt.R")
load("./data/processed_long_dat.Rdata")
load("./data/processed_dat.Rdata")

lrt_stats <- matrix(NA, nrow = nrow(new_cases), ncol = 1124)

for (j in 1:nrow(new_cases)){
  
  test = new_cases[j,]
  
  lrt_stat <- c()
  
  for (i in 30:(length(test) - 30 + 1)){
    lrt_stat = c(lrt_stat, tryCatch(lrt(y1 = test[(i - 29):i], y2 = test[(i + 1):(i + 30)], 
                                        s1 = populations$population[1], 
                                        s2 = populations$population[1],
                                        i1 = (i - 29):i,
                                        i2 = (i + 1):(i + 30), 
                                        df1 = 3, 
                                        df2 = 3)$lambda, error = function(e) return(NA)))
  }
  
  lrt_stats[j,] <- lrt_stat
}

# Produce figure for one county - use first full time series

test <- new_cases[1,]

lrt_stat <- c()

for (i in 30:(length(test) - 30 + 1)){
  lrt_stat <- c(lrt_stat, tryCatch(lrt(y1 = test[(i - 29):i], y2 = test[(i + 1):(i + 30)], 
                           s1 = populations$population[1], 
                           s2 = populations$population[1],
                           i1 = (i - 29):i,
                           i2 = (i + 1):(i + 30), 
                           df1 = 3, 
                           df2 = 3)$lambda, error = function(e) return(NA)))
}
dates <- dates[30:(length(test) - 30 + 1)]

filename <- "figures/LRT_scan_figure.pdf"

pdf(filename, width = 6, height = 6)

plot(lrt_stat ~ dates, type = "l", xlab = "Date", ylab = "LRT Statistic")

plot(test[30:(length(test) - 30 + 1)] ~ dates, col = "red", type = "l")

abline(h = quantile(lrt_stat, probs = 0.95, na.rm =TRUE), col = "black")

abline(h = qchisq(0.95, 1), col = "blue")

dev.off()