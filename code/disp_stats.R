# Compute dispersion stat (deviance divided by degrees of freedom) for each county

library(splines)

load("data/processed_dat.Rdata")

disp_stats <- rep(NA, times = nrow(new_cases_subset))

for (i in 1:nrow(new_cases_subset)){
  
  y = new_cases_subset[i,]
  
  inds = 1:length(y)
  
  pop = rep(populations_subset$population[i], times = length(y))
  
  pois.fit <- glm(y ~ ns(inds, df = 3, intercept = TRUE) + offset(log(pop)), 
                  family = poisson)
  
  disp_stats[i] = pois.fit$deviance/(length(y) - 3)
  
}

filename <- "data/dispersion_stats.Rdata"

save(disp_stats, file = filename)
