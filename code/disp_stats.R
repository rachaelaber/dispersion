# Compute dispersion stat for each county

disp_stats <- rep(NA, times = nrow(new_cases_subset))

for (i in 1:nrow(new_cases_subset)){
  
  y = new_cases_subset[i,]
  
  inds = 1:length(y)
  
  pop = rep(populations$population[i], times = length(y))
  
  pois.fit <- glm(y ~ ns(inds, df = 3, intercept = TRUE) + offset(log(pop)), 
                  family = poisson)
  print(pois.fit)
  #disp_stats[i] = pois.fit$deviance/(length(y) - 3)
  
}