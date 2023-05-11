# Compute p-values for a state's counties 
source("code/lrt.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

state <- "NY"

# Loop through counties and calculate p values
state_index <- which(populations$State == state)
pvals <- rep(NA, times = length(state_index))

for (i in state_index){
  pvals[i] <- lrt(y1 = new_cases_subset[i,][1:30], y2 = new_cases_subset[i,][31:60], 
                  s1 = populations$population[i], s2 = populations$population[i], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$p
}

# Save NY p-values
filename <- "data/LRT_pvals_NY.Rdata"
save(pvals, file = filename)