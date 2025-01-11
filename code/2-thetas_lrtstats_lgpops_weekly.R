# Compute thetas for large counties over time
rm(list = ls())

source("code/lrt.R")
source("code/dispersion_test.R")
load("data/processed/nyt_weekly.Rdata")


# Parameters
ww <- 8
df <- 3
ptol <- 1e-4
ztol <- 1e4


# Calculated quantities
ncounty <- nrow(cases)
nweek <- ncol(cases)
nestimate <- length(ww:(nweek - ww))


# Set up empty matrices and loop through counties and time points
lrt_stats <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
lrt_ps <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
thetas1 <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
thetas2 <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
thetas <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
ftr_poiss <- matrix(NA, nrow = nrow(cases), ncol = nestimate)
ctzs <- matrix(NA, nrow = nrow(cases), ncol = nestimate)

for (j in seq_len(nrow(cases))) {
  print(paste("County", j, "of", nrow(cases)))

  series <- cases[j, ]

  lrt_stat <- c()
  lrt_p <- c()
  theta1 <- c()
  theta2 <- c()
  theta <- c()
  ftr_pois <- c()
  ctz <- c()

  for (i in ww:(nweek - ww)) {
    
    y <- series[(i-ww+1):(i + ww)]
    
    out <- dispersion_test(y = y, s = pops[j], df = df, ptol = ptol , ztol = ztol)
    print(out)

    theta1 <- c(theta1, out$theta1_est)
    theta2 <- c(theta2,  out$theta2_est)
    theta <- c(theta,  out$theta0_est)
    lrt_stat <- c(lrt_stat, out$lambda)
    lrt_p <- c(lrt_p, out$p)
    ftr_pois <- c(ftr_pois, out$fail_to_reject_poisson)
    ctz <- c(ctz, out$collapse_to_zero)

    }

 lrt_stats[j, ] <- lrt_stat
 lrt_ps[j, ] <- lrt_p
 thetas1[j, ] <- theta1
 thetas2[j, ] <- theta2
 thetas[j, ] <- theta
 ftr_poiss[j, ] <- ftr_pois
 ctzs[j, ] <- ctz
 
}

filename <- "data/processed/theta_lg_pops.Rdata"

save(thetas1, thetas2, thetas, file = filename)

filename <- "data/processed/lrt_lg_pops.Rdata"

save(lrt_stats, file = filename)

filename <- "data/processed/lrtps_lg_pops.Rdata"

save(lrt_ps, file = filename)

filename <- "data/processed/ftr_poiss_lg_pops.Rdata"

save(ftr_poiss, file = filename)

filename <- "data/processed/ctzs_lg_pops.Rdata"

save(ctzs, file = filename)
