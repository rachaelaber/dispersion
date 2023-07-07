source("code/W.R")
load("data/processed_dat.Rdata")

diagnostic <- function(y = new_cases_subset[i, ], 
                       population = populations$population[i]){
  # Diagnostic plots
  
  day = 1:length(y)

  #plot(day, y)
  
  f1 = my_spl_fit(y, population, day, df = 3);
  
  #lines(day, f1$mu)
  
  #
  
  r = y - f1$mu;
  
  # E(r^2) = mu + phi * mu^2
  # E(r^2) = mu + phi * mu^p
  
  x = r^2 - y;
  
  ## E(X) = phi * mu^p
  ## log(E(X)) ~ log(phi) + p log(E(Y))
  
  logx = log(x);
  logy = log(y);
  
  s = x > 0 & y > 0;
  #plot(logy[s], logx[s]);
  
  plot(log(y + 1e-10), log(x))
  abline(log(0.1), 2)
  
  #m = lm(logx ~ logy, subset = x > 0 & y > 0)
  #summary(m)
  
}

filename <- "figures/diagnostic_plot.pdf"

pdf(filename, width = 6, height = 6)

par(mfrow = c(2,2))

for (i in c(2, 40, 300, 777)){
  diagnostic()
}

dev.off()
