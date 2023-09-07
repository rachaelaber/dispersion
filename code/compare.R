
load("data/processed_long_dat.Rdata")

load("data/lrt_lg_pops.Rdata")
load("data/thetadiff_lg_pops.Rdata")
load("data/theta_lg_pops.Rdata")


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

filename <- "figures/compare.pdf"

pdf(filename, height = 8, width = 8)

for (i in 1:51){
  
  series = new_cases_lg[i,]
  
  par(mfrow = c(2, 2))
  plot(series[30:(length(series) - 30 + 1)], type = "l", main = "Incidence")
  plot(lrt_stats[i, ], type = "l", col = 2, main = "LRT")
  plot(thetadiffs[i, ], type = "l", col = 3, main = "Theta Difference")
  plot(thetas[i, ], type = "l", col = 4, main = "Theta")
}

dev.off()
