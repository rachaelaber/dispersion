# Note that the first 24 are omitted in these plots

load("data/processed_long_dat.Rdata")

load("data/new_cases_lg.Rdata")

load("data/lrt_lg_pops.Rdata")

load("data/theta_lg_pops.Rdata")

dates <- dates[30:(length(new_cases[1,]) - 30 + 1)]

filename <- "figures/compare.png"

png(filename)

for (i in 1:1){ # 1:length(populations_lg$population)d
  
  series = new_cases_lg[i,]
  
  par(mfrow = c(3, 1))
  
  plot(series[(30 + 35):(length(series) - 30 + 1)], type = "h", main = "Incidence")
  
  plot(lrt_stats[i, 35:dim(lrt_stats)[2]], type = "h", col = 2, main = "LRT")
  
  plot(log(thetas[i, 35:dim(thetas)[2]]), type = "l", col = 4, main = "Log Theta")
  
}

dev.off()
