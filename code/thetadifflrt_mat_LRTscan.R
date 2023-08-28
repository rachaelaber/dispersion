# Compute LRT scan statistics and thetadiffs for large counties

source("code/W.R")

source("code/lrt.R")

load("data/processed_long_dat.Rdata")

# Indices for largest county in each state

unique_states <- unique(populations$State)

nstate <- length(unique_states)

keep <- rep(NA, times = nstate)

for (i in 1:nstate){
  
  j <- which(populations$State == unique_states[i])
  
  k <- which.max(populations$population[j])
  
  keep[i] <- j[k]
}

new_cases_lg <- new_cases[keep,]

populations_lg <- populations[keep,]


# Set up empty matrices 

lrt_stats <- matrix(NA, nrow = 51, ncol = 1124)

thetadiffs <- matrix(NA, nrow = 51, ncol = 1124)


for (j in 1:nstate){
  
  series = new_cases_lg[j,]
  
  lrt_stat <- c()
  
  thetadiff <- c()
  
  for (i in 30:(length(series) - 30 + 1)){
    
    Y = series[(i - 29):(i + 30)]
    
    test = tryCatch(lrt(y1 = Y[1:(length(Y)/2)], y2 = Y[((length(Y)/2) + 1):length(Y)],
                        s1 = populations_lg$population[j],
                        s2 = populations_lg$population[j],
                        i1 = 1:30,
                        i2 = 31:60,
                        df1 = 3,
                        df2 = 3)$lambda, error = function(e) return(NA))
    
    lrt_stat = c(lrt_stat, test)
    
    test.1 = tryCatch(W(y = Y, population_size = populations_lg$population[j], 
                        breakpoint = 30, deg_free = 3, 
                        fn = my_spl_fit, verbose = FALSE, return_theta_diff = TRUE),
                      error = function(e) return(NA))
    
    thetadiff = c(thetadiff, test.1)
    
  }
  
  lrt_stats[j,] <- lrt_stat
  
  thetadiffs[j,] <- thetadiff
  
}

# Save LRT statistic array; save thetadiff array

filename <- "data/lrt_lg_pops.Rdata"

save(lrt_stats, file = filename)

filename <- "data/thetadiff_lg_pops.Rdata"

save(thetadiffs, file = filename)

# Load data

load("data/lrt_lg_pops.Rdata")

load("data/processed_long_dat.Rdata")  

# Plot

this_series <- new_cases_lg[1, ]

dates <- dates[30:(length(this_series) - 30 + 1)]

filename <- "figures/LRT_scan_figure.pdf"

pdf(filename, width = 3, height = 3)

par(mfrow = c(2, 1))

plot(lrt_stats[1, ] ~ dates, type = "l", xlab = "Date", ylab = "LRT Statistic")

plot(this_series[30:(length(this_series) - 30 + 1)] ~ dates, col = "red", type = "l")

abline(h = quantile(lrt_stats[1, ], probs = 0.95, na.rm =TRUE), col = "black")

abline(h = qchisq(0.95, 1), col = "blue")

dev.off()
