
load("data/processed_long_dat.Rdata")
dates <- dates[30:(length(new_cases[1,]) - 30 + 1)]

#load("data/lrt_lg_pops.Rdata")
#load("data/thetadiff_lg_pops.Rdata")
load("data/theta_lg_pops.Rdata")


# Indices for largest counties in each state

unique_states <- unique(populations$State)

nstate <- length(unique_states)

keep <- c()

for (i in 1:nstate){
  
  j <- which(populations$State == unique_states[i])
  
  k <- which(populations$population[j] > quantile(populations$population[j], 0.96))
  
  keep <- c(keep, j[k])
}

new_cases_lg <- new_cases[keep,]

populations_lg <- populations[keep,]

filename <- "figures/compare1.pdf"

pdf(filename, height = 8, width = 8)

for (i in 1:length(keep)){
  
  series = new_cases_lg[i,]
  par(mfrow = c(2, 1))
  plot(series[30:(length(series) - 30 + 1)], type = "l", main = "Incidence")
  abline(v = 281, col = "red")
  #plot(lrt_stats[i, ], type = "l", col = 2, main = "LRT")
  #plot(thetadiffs[i, ], type = "l", col = 3, main = "Theta Difference")
  plot(log(thetas[i, ]), type = "l", col = 4, main = "Theta")
  abline(v = 281, col = "red")
}

dev.off()

# Is the discrepancy in theta b/w two time periods different for the larger and smaller
# population halves?

thetasdiff = thetas[ ,380] - thetas[ ,250]

thetasdiff_small <- thetasdiff[populations_lg$population <= 
                                 quantile(populations_lg$population, 0.5)]

thetasdiff_large <- thetasdiff[populations_lg$population >
                                 quantile(populations_lg$population, 0.5)]

mean(thetasdiff_small, na.rm = TRUE)

mean(thetasdiff_large, na.rm = TRUE)
