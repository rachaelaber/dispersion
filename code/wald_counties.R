# Compute p-values for a state's counties 
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

state <- "NY"

# Loop through counties and calculate p values
state_index <- which(populations$State == state)
pvals <- rep(NA, times = length(state_index))

for (i in state_index){
  pvals[i] <- W(new_cases_subset[i,], population_size = populations$population[i], breakpoint = 30, 
                deg_free = 3, fn = my_spl_fit, verbose = FALSE)
}

# Save NY p-values
filename <- "data/W_pvals_NY.Rdata"
save(pvals, file = filename)

# Visualize p-values
hist(pvals, col=2)