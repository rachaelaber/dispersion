# Compute p-values for a state's counties 
source("code/lrt.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p values
state_index <- which(populations$State == state)

pvals <- rep(NA, times = length(state_index))

for (i in 1:length(state_index)){
  pvals[i] <- lrt(y1 = new_cases_subset[state_index[i],][1:30], y2 = new_cases_subset[state_index[i],][31:60], 
                  s1 = populations$population[state_index[i]], s2 = populations$population[state_index[i]], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$p
}

# Save p-values
filename <- paste("data/LRT_pvals", state, ".Rdata", sep = "")
save(pvals, file = filename)

# Visualize p-values
hist(pvals, col=2)