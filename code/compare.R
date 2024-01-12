
load("data/processed_long_dat.Rdata")

load("data/new_cases_lg.Rdata")

load("data/lrt_lg_pops.Rdata")

load("data/theta_lg_pops.Rdata")

dates <- dates[30:(length(new_cases[1,]) - 30 + 1)]

filename <- "figures/compare.pdf"

pdf(filename, height = 6, width = 6)

for (i in 1:length(populations_lg$population)){
  
  series = new_cases_lg[i,]
  
  par(mfrow = c(2, 1))
  
  plot(series[30:(length(series) - 30 + 1)], type = "l", main = "Incidence")
  
  abline(v = 281, col = "red")
  
  plot(lrt_stats[i, ], type = "l", col = 2, main = "LRT")
  
  plot(log(thetas[i, ]), type = "l", col = 4, main = "Log Theta")
  
  abline(v = 281, col = "red")
}

dev.off()
