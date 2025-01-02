rm(list = ls())
graphics.off()



# Parameters
df <- 3
ww <- 8
ptol <- 1e-3
ztol <- 1e3


# Load data and functions
source("code/lrt.R")
load("data/processed/nyt_weekly.Rdata")



# Construct smoothed incidence series
# based on US national COVID-19 incidence
# to use for the underlying trends
cases_smooth <- smooth.spline(colSums(cases), df = 32)$y
incidence_smooth <- cases_smooth / sum(pops)


# Function to generate and fit a sample trajectory
sim <- function(theta1, theta2, s) {
  u <- 25:(144 - 2 * ww)
  a <- sample(u, 1)
  b <- a + 2 * ww
  incidence_sample <- incidence_smooth[a:b]

  mu <- incidence_sample * s

  i1 <- 1:ww
  i2 <- (ww + 1):(2 * ww)

  mu1 <- mu[i1]
  mu2 <- mu[i2]

  y1 <- rnbinom(ww, size = theta1, mu = mu1)
  y2 <- rnbinom(ww, size = theta2, mu = mu2)

  out <- tryCatch(lrt( # nolint
    y1 = y1,
    y2 = y2,
    s1 = s,
    s2 = s,
    i1 = i1,
    i2 = i2,
    df1 = df,
    df2 = df,
    ptol = ptol,
    ztol = ztol
  ), error = function(e) {
    return(NA)
  })

  if (!is.list(out)) {
    theta1_est <- NA
    theta2_est <- NA
    p <- NA
    fail_to_reject_poisson <- NA
    collapse_to_zero <- NA
  } else {
    theta1_est <- 1 / out$phi11
    theta2_est <- 1 / out$phi12
    p <- out$p
    fail_to_reject_poisson <- out$fail_to_reject_poisson
    collapse_to_zero <- out$collapse_to_zero
  }

  return(list(
    p = p,
    theta1_est = theta1_est,
    theta2_est = theta2_est,
    i1 = i1,
    i2 = i2,
    y1 = y1,
    y2 = y2,
    mu1 = mu1,
    mu2 = mu2,
    fail_to_reject_poisson = fail_to_reject_poisson,
    collapse_to_zero = collapse_to_zero
  ))
}



# Apply
nsim <- 10^2
simdata <- data.frame(s = 10 ^ runif(nsim, 3, 7),
                      theta1 = 10 ^ runif(nsim, -2, 2),
                      theta2 = 10 ^ runif(nsim, -2, 2))
simdata$theta1_est <- NA
simdata$theta2_est <- NA
simdata$p <- NA
simdata$fail_to_reject_poisson <- NA
simdata$collapse_to_zero <- NA

for (i in seq_len(nrow(simdata))) {
  out <- sim(
    simdata$theta1[i],
    simdata$theta2[i],
    simdata$s[i]
  )

  simdata$theta1_est[i] <- out$theta1_est
  simdata$theta2_est[i] <- out$theta2_est
  simdata$p[i] <- out$p
  simdata$fail_to_reject_poisson[i] <- out$fail_to_reject_poisson
  simdata$collapse_to_zero[i] <- out$collapse_to_zero
}




#
par(mfrow = c(2, 2))
par(cex = 2)
par(mar = c(5, 5, 3, 1))
plot(
  x = log10(simdata$theta1),
  y = log10(simdata$theta1_est),
  xlab = expression(log[10] ~ theta[1]),
  ylab = expression(log[10] ~ hat(theta[1])),
  pch = 19,
  col = ifelse(simdata$fail_to_reject_poisson | simdata$collapse_to_zero, 2, 1)
)
abline(0, 1, col = 4, lwd = 4)
