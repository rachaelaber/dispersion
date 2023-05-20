# Compute p-values for a state's counties 
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p values
state_index <- which(populations$State == state)

pvals <- rep(NA, times = length(state_index))

for (i in 1:length(state_index)){
  pvals[i] <- tryCatch(W(new_cases_subset[state_index[i],], 
                         population_size = populations$population[state_index[i]],
                         breakpoint = 30, deg_free = 3, 
                         fn = my_spl_fit, verbose = FALSE), 
                       error = function(e) return(NA))
}

# Save p-values
filename <- paste("data/W_pvals", state, ".Rdata", sep ="")
save(pvals, file = filename)

# Visualize p-values
hist(pvals, col=2)