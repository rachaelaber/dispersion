# Compute LRT scan statistics and thetadiffs for large counties

library(dplyr)

source("code/W.R")

source("code/lrt.R")

load("data/processed_long_dat.Rdata")

# Subset case data to largest population county per state

new_cases_lg <- cbind(new_cases, populations)

new_cases_lg <- new_cases_lg %>% group_by(State) %>% slice_max(n = 1, population)

# No longer need metadata columns in same dataframe

populations_lg <- subset(new_cases_lg, select = c(countyFIPS, County.Name, State, population))
  
new_cases_lg <- subset(new_cases_lg, select = -c(countyFIPS, County.Name, State, population))

new_cases_lg <- as.matrix(new_cases_lg)

# Set up empty matrices 

lrt_stats <- matrix(NA, nrow = 51, ncol = 1124)

thetadiffs <- matrix(NA, nrow = 51, ncol = 1124)

for (j in 1:51){
  
  print(j)
  
  series = new_cases_lg[j,]
  
  lrt_stat <- c()
  
  thetadiff <- c()
  
  for (i in 30:(length(series) - 30 + 1)){
    
    Y = series[(i - 29):(i + 30)]
    
    lrt_stat = c(lrt_stat, tryCatch(lrt(y1 = Y[1:(length(Y)/2)], y2 = Y[((length(Y)/2) + 1):length(Y)], 
                                        s1 = populations_lg$population[j], 
                                        s2 = populations_lg$population[j],
                                        i1 = (i - 29):i,
                                        i2 = (i + 1):(i + 30), 
                                        df1 = 3, 
                                        df2 = 3)$lambda, error = function(e) return(NA)))
    
    thetadiff = c(thetadiff, tryCatch(W(y = Y, 
                  population_size = populations_lg$population[j], 
                  breakpoint = i, deg_free = 3, 
                  fn = my_spl_fit, verbose = FALSE, return_theta_diff = TRUE),
                  error = function(e){
                    return(NA)
                  }))
    
  }
  
  lrt_stats[j,] <- lrt_stat
  
  thetadiffs[j,] <- thetadiff
  
}

# Save LRT statistic array; save thetadiff array

filename <- "data/lrt_lg_pops.Rdata"

save(lrt_stats, file = filename)

filename <- "data/thetadiff_lg_pops.Rdata"

save(thetadiffs, file = filename)

# # Produce figure for one county - use first (of large populations) full time series 
# 
# this_series <- new_cases_lg[1, ]
# 
# dates <- dates[30:(length(this_series) - 30 + 1)]
# 
# filename <- "figures/LRT_scan_figure.pdf"
# 
# pdf(filename, width = 6, height = 6)
# 
# plot(lrt_stats[1, ] ~ dates, type = "l", xlab = "Date", ylab = "LRT Statistic")
# 
# plot(this_series[30:(length(this_series) - 30 + 1)] ~ dates, col = "red", type = "l")
# 
# abline(h = quantile(lrt_stats[1, ], probs = 0.95, na.rm =TRUE), col = "black")
# 
# abline(h = qchisq(0.95, 1), col = "blue")
# 
# dev.off()
# 
# # Create surfaces 
# 
# # Plot likelihood ratios as a surface over counties and time
# 
# rm(list = ls())
# 
# graphics.off()
# 
# library(viridis)
# 
# 
# # Load data
# 
# load("data/lrt_lg_pops.Rdata")
# 
# load("data/processed_long_dat.Rdata")   # for dates
# 
# # Data setup
# dates <- dates[30:(length(dates) - 30 + 1)]
# z <- t(lrt_stats)
# x <- seq_len(nrow(z))
# y <- seq_len(ncol(z))
# 
# filename <- "figures/lrt_surface_figure.pdf"
# 
# pdf(filename, width = 8, height = 8)
# 
# # Plot
# par(pin = c(7, 5))
# image(x, y, z,
#       xaxt = "n",
#       yaxt = "n",
#       xlab = "",
#       ylab = "Locations",
#       col = rev(mako(32)))
# 
# par(las = 2)
# axis(1, x, dates, tick = FALSE)
# 
# dev.off()
# 
# # Dates of bands: 
# # Just after 2021-03-01; just after 2021-05-15; 2021-04-05
# # 2021-01-10 seen across many of the counties
# 
# # Color by direction of theta change by making same matrix w/
# # theta change
# 
# load("data/thetadiff_lg_pops.Rdata")
# 
# z <- t(thetadiffs)
# x <- seq_len(nrow(z))
# y <- seq_len(ncol(z))
# 
# filename <- "figures/thetadiff_surface_figure.pdf"
# 
# pdf(filename, width = 8, height = 8)
# 
# # Plot
# par(pin = c(7, 5))
# image(x, y, z,
#       xaxt = "n",
#       yaxt = "n",
#       xlab = "",
#       ylab = "Locations",
#       col = rev(mako(32)))
# 
# par(las = 2)
# axis(1, x, dates, tick = FALSE)
# 
# dev.off()