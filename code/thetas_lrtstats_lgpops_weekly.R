# Compute thetas and LRT statistics for large counties over time

rm(list = ls())
graphics.off()

source("code/lrt.R")

source("code/my_spl_fit.R")

load("data/processed_long_dat.Rdata")

# Indices for largest counties in each state

unique_states <- unique(populations$State)

nstate <- length(unique_states)

keep <- c()

for (i in 1:nstate) {
  j <- which(populations$State == unique_states[i])

  k <- which(populations$population[j] > quantile(populations$population[j], 0.96))

  keep <- c(keep, j[k])
}

new_cases_lg <- new_cases[keep, ]

populations_lg <- populations[keep, ]

# Convert to weekly data
new_cases_lg_weekly <- matrix(NA, nrow(new_cases_lg), ncol(new_cases_lg))
for (i in seq_len(nrow(new_cases_lg))) {
  new_cases_lg_weekly[i, ] <- filter(new_cases_lg[i, ], rep(1, 7), sides = 1)
}

# Replace NAs with 0s
new_cases_lg_weekly[is.na(new_cases_lg_weekly)] <- 0


# Save
filename <- "data/new_cases_lg_weekly.Rdata"

save(new_cases_lg, new_cases_lg_weekly, populations_lg, file = filename)

# Set up empty matrices and loop through counties/time points

lrt_stats <- matrix(NA, nrow = length(keep), ncol = 1124)
lrt_ps <- matrix(NA, nrow = length(keep), ncol = 1124)
thetas <- matrix(NA, nrow = length(keep), ncol = 2248)

for (j in 1:length(keep)) {
  print(paste("County", j, "of", length(keep)))

  series <- new_cases_lg_weekly[j, ]

  lrt_stat <- c()
  lrt_p <- c()
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
        theta <- c(theta, 1 / out$phi11, 1 / out$phi12)
        lrt_stat <- c(lrt_stat, out$lambda)
        lrt_p <- c(lrt_p, out$p)
  
    }else{
      lrt_stat <- c(lrt_stat, NA)
      lrt_p <- c(lrt_p, NA)
      theta <- c(theta, NA, NA)
    }
  }

  lrt_stats[j, ] <- lrt_stat
  lrt_ps[j, ] <- lrt_p
  thetas[j, ] <- theta
}

# Save (drop entries with unreasonably small theta)

filename <- "data/theta_lg_pops_weekly.Rdata"

thetas[thetas < 1e-10] <- NA

save(thetas, file = filename)

# Make thetas the same dim as the other two dataframes for subsetting

comb_df <- matrix(nrow = nrow(thetas), ncol = ncol(thetas)/2)

comb_df[, 1] <- paste(thetas[ , 1], thetas[ , 2], sep = ",")

odds <- seq(1, ncol(thetas), by = 2)

evs  <- seq(2, ncol(thetas), by = 2)

for (c in 1:(ncol(thetas)/2)) {
  
  comb_df[, c] = paste(thetas[ , odds[c]], thetas[ , evs[c]], sep = ",")
}

comb_df <- as.data.frame(comb_df)

filename <- "data/lrt_lg_pops_weekly.Rdata"

lrt_stats[] <- NA

save(lrt_stats, file = filename)

filename <- "data/lrtps_lg_pops_weekly.Rdata"

lrt_ps[] <- NA

save(lrt_ps, file = filename)