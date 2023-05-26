# Compute p-values for counties 
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p-values

pvals <- rep(NA, times = dim(new_cases_subset)[1])

for (i in 1:dim(new_cases_subset)[1]){
  pvals[i] <- tryCatch(W(new_cases_subset[i,], 
                         population_size = populations$population[i],
                         breakpoint = 30, deg_free = 3, 
                         fn = my_spl_fit, verbose = FALSE), 
                       error = function(e) return(NA))
}

# Save p-values
filename <- "data/W_pvals"
save(pvals, file = filename)

# Visualize p-values
hist(pvals, col=2)
