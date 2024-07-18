# Compute thetas and LRT statistics for large counties over time
rm(list = ls())

source("code/lrt.R")

load("data/processed/nyt_weekly.Rdata")

# Set up empty matrices and loop through counties and time points
lrt_stats <- matrix(NA, nrow = nrow(cases), ncol = 154)
lrt_ps <- matrix(NA, nrow = nrow(cases), ncol = 154)
thetas1 <- matrix(NA, nrow = nrow(cases), ncol = 154)
thetas2 <- matrix(NA, nrow = nrow(cases), ncol = 154)
thetas <- matrix(NA, nrow = nrow(cases), ncol = 154)

for (j in 1:nrow(cases)) {
  
  print(paste("County", j, "of", nrow(cases)))

  series <- cases[j, ]

  lrt_stat <- c()
  lrt_p <- c()
  theta1 <- c()
  theta2 <- c()
  theta <- c()

  for (i in 8:(length(series) - 8 + 1)) {
    Y <- series[(i - 8):(i + 8)]

    out <- tryCatch(lrt(
      y1 = Y[1:(length(Y) / 2)], y2 = Y[((length(Y) / 2) + 1):length(Y)],
      s1 = pops[j],
      s2 = pops[j],
      i1 = 1:8,
      i2 = 9:16,
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

filename <- "data/processed/theta_lg_pops.Rdata"

# thetas1[bad_indices] <- NA
# 
# thetas2[bad_indices] <- NA
# 
# thetas[bad_indices2] <- NA

save(thetas1, thetas2, thetas, file = filename)

filename <- "data/processed/lrt_lg_pops.Rdata"

# lrt_stats[bad_indices] <- NA

save(lrt_stats, file = filename)

filename <- "data/processed/lrtps_lg_pops.Rdata"

# lrt_ps[bad_indices] <- NA

save(lrt_ps, file = filename)

