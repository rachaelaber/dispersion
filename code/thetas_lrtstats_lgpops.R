# Compute thetas and LRT statistics for large counties over time

source("code/lrt.R")

source("code/my_spl_fit.R")

load("data/processed_long_dat.Rdata")

# Indices for largest counties in each state

unique_states <- unique(populations$State)

nstate <- length(unique_states)

keep <- c()

for (i in 1:nstate){
  
  j <- which(populations$State == unique_states[i])
  
  k <- which(populations$population[j] > quantile(populations$population[j], 0.96))
  
  keep <- c(keep, j[k])
}

new_cases_lg <- new_cases[keep,]

populations_lg <- populations[keep,]

filename <- "data/new_cases_lg.Rdata"

save(new_cases_lg, populations_lg, file = filename)

# Set up empty matrices and loop through counties/time points

lrt_stats <- matrix(NA, nrow = length(keep), ncol = 1124)

thetas <- matrix(NA, nrow = length(keep), ncol = 1124)

for (j in 1:length(keep)){
  
  series = new_cases_lg[j,]
  
  lrt_stat = c()

  theta = c()
  
  for (i in 30:(length(series) - 30 + 1)){
    
    Y = series[(i - 29):(i + 30)]
    
    test = tryCatch(lrt(y1 = Y[1:(length(Y)/2)], y2 = Y[((length(Y)/2) + 1):length(Y)],
                        s1 = populations_lg$population[j],
                        s2 = populations_lg$population[j],
                        i1 = 1:30,
                        i2 = 31:60,
                        df1 = 3,
                        df2 = 3)$lambda, error = function(e) return(NA))

    lrt_stat = c(lrt_stat, test)


    test.2 = tryCatch(lrt(y1 = Y[1:(length(Y)/2)], y2 = Y[((length(Y)/2) + 1):length(Y)],
                          s1 = populations_lg$population[j],
                          s2 = populations_lg$population[j],
                          i1 = 1:30,
                          i2 = 31:60,
                          df1 = 3,
                          df2 = 3)$phi11, error = function(e) return(NA))

    theta = c(theta, 1/test.2)
    
  }

   lrt_stats[j,] <- lrt_stat

   thetas[j,] <- theta
   
}

# Save 

filename <- "data/lrt_lg_pops.Rdata"

save(lrt_stats, file = filename)

filename <- "data/theta_lg_pops.Rdata"

save(thetas, file = filename)
