# Compute thetas and LRT statistics for large counties over time
rm(list = ls())

source("code/lrt.R")

source("code/my_spl_fit.R")

load("data/processed/new_cases_lg_weekly.Rdata")

# Set up empty matrices and loop through counties and time points
lrt_stats <- matrix(NA, nrow = nrow(new_cases_lg_weekly), ncol = 1124)
lrt_ps <- matrix(NA, nrow = nrow(new_cases_lg_weekly), ncol = 1124)
thetas1 <- matrix(NA, nrow = nrow(new_cases_lg_weekly), ncol = 1124)
thetas2 <- matrix(NA, nrow = nrow(new_cases_lg_weekly), ncol = 1124)
thetas <- matrix(NA, nrow = nrow(new_cases_lg_weekly), ncol = 1124)

for (j in 1:nrow(new_cases_lg_weekly)) {
  
  print(paste("County", j, "of", nrow(new_cases_lg_weekly)))

  series <- new_cases_lg_weekly[j, ]

  lrt_stat <- c()
  lrt_p <- c()
  theta1 <- c()
  theta2 <- c()
  theta <- c()

  for (i in 30:(length(series) - 30 + 1)) {
    Y <- series[(i - 29):(i + 30)]

    out <- tryCatch(lrt(
      y1 = Y[1:(length(Y) / 2)], y2 = Y[((length(Y) / 2) + 1):length(Y)],
      s1 = populations_lg$population[j],
      s2 = populations_lg$population[j],
      i1 = 1:30,
      i2 = 31:60,
      df1 = 3,
      df2 = 3
    ), error = function(e) {
      return(NA)
    })

    if (!is.na(out[1])) {
        theta1 <- c(theta1, 1 / out$phi11)
        theta2 <- c(theta2, 1 / out$phi12)
        theta <- c(theta, 1/ out$phi0)
        lrt_stat <- c(lrt_stat, out$lambda)
        lrt_p <- c(lrt_p, out$p)
  
    }else{
      lrt_stat <- c(lrt_stat, NA)
      lrt_p <- c(lrt_p, NA)
      theta1 <- c(theta1, NA)
      theta2 <- c(theta2, NA) 
      theta <- c(theta, NA)
    }
  }

  lrt_stats[j, ] <- lrt_stat
  lrt_ps[j, ] <- lrt_p
  thetas1[j, ] <- theta1
  thetas2[j, ] <- theta2
  thetas[j, ] <- theta
  
}

# Save/drop indices with theta too small
# bad_indices <- thetas1 < 1e-10 | thetas2 < 1e-10
# 
# bad_indices2 <- thetas < 1e-10

filename <- "data/processed/theta_lg_pops_weekly.Rdata"

# thetas1[bad_indices] <- NA
# 
# thetas2[bad_indices] <- NA
# 
# thetas[bad_indices2] <- NA

save(thetas1, thetas2, thetas, file = filename)

filename <- "data/processed/lrt_lg_pops_weekly.Rdata"

# lrt_stats[bad_indices] <- NA

save(lrt_stats, file = filename)

filename <- "data/processed/lrtps_lg_pops_weekly.Rdata"

# lrt_ps[bad_indices] <- NA

save(lrt_ps, file = filename)
