rm(list = ls())
graphics.off()



# Parameters
df <- 3
ww <- 8



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
    df2 = df
  ), error = function(e) {
    return(NA)
  })

  if (is.na(out)[1]) {
    theta1_est <- NA
    theta2_est <- NA
    p <- NA
  } else {
    theta1_est <- 1 / out$phi11
    theta2_est <- 1 / out$phi12
    p <- out$p
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
    mu2 = mu2
  ))
}



# Apply
s_lev <- seq(10^3, 10^7, length.out = 10)
theta_lev <- 10 ^ seq(-1, 2, length.out = 10)
simdata <- expand.grid(s = s_lev, theta1 = theta_lev, theta2 = theta_lev)
simdata$theta1_est <- NA
simdata$theta2_est <- NA
simdata$p <- NA

for (i in seq_len(nrow(simdata))) {
  out <- sim(
    simdata$theta1[i],
    simdata$theta2[i],
    simdata$s[i]
  )

  simdata$theta1_est[i] <- out$theta1_est
  simdata$theta2_est[i] <- out$theta2_est
  simdata$p[i] <- out$p
}



# TEMP
plot(c(out$i1, out$i2), c(out$y1, out$y2), cex = 5)
lines(c(out$i1, out$i2), c(out$mu1, out$mu2), lwd = 3, col = 2)




#
par(cex = 3)
plot(
  log10(simdata$theta1),
  log10(simdata$theta1_est),
  cex = log10(simdata$s) / 3
)
abline(0, 1, col = 2, lwd = 8)
