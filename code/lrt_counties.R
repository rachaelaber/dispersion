# Compute p-values for counties 
source("code/lrt.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p-values

pvals <- rep(NA, times = dim(new_cases_subset)[1])

for (i in 1:dim(new_cases_subset)[1]){
  pvals[i] <- tryCatch(lrt(y1 = new_cases_subset[i,][1:30], y2 = new_cases_subset[i,][31:60], 
                  s1 = populations$population[i], s2 = populations$population[i], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$p, error = function(e) return(NA))
}

# Save p-values
filename <- "data/LRT_pvals"
save(pvals, file = filename)

# Visualize p-values
hist(pvals, col=2)
