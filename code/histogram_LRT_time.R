# Find the approx date of the max lrt stat for each large county 

load("data/lrt_lg_pops.Rdata")

max_inds <- rep(NA, times = nrow(lrt_stats))

for (j in 1:nrow(lrt_stats)){
  max_inds[j] = which.max(lrt_stats[j, ])
} 

# Histogram

hist(max_inds) 
# seems like many max stats are around index 300 (index 281 is Thanksgiving 2020)
