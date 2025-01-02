rm(list = ls())
graphics.off()



# Parameters
df <- 3
ww <- 8
theta <- 10
s <- 10^4


# Load data and functions
source("code/lrt.R")
load("data/processed/nyt_weekly.Rdata")



# Construct smoothed incidence series
# based on US national COVID-19 incidence
# to use for the underlying trends
cases_smooth <- smooth.spline(colSums(cases), df = 32)$y
incidence_smooth <- cases_smooth / sum(pops)




# Generate a sample trajectory
sample_trajectory <- function(theta1, theta2, s) {
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

  return(list(y1 = y1, y2 = y2, i1 = i1, i2 = i2, mu1 = mu1, mu2 = mu2))
}


# Visualize likelihood surfaces
out <- sample_trajectory(theta1 = theta, theta2 = theta, s = s)
y1 <- out$y1
y2 <- out$y2
i1 <- out$i1
i2 <- out$i2
mu1 <- out$mu1
mu2 <- out$mu2


#
surfdata <- lrt( # nolint
  y1 = y1,
  y2 = y2,
  s1 = s,
  s2 = s,
  i1 = i1,
  i2 = i2,
  df1 = df,
  df2 = df,
  surface = TRUE
)



#
fitdata <- lrt( # nolint
  y1 = y1,
  y2 = y2,
  s1 = s,
  s2 = s,
  i1 = i1,
  i2 = i2,
  df1 = df,
  df2 = df,
  surface = FALSE,
  ptol = 1e-4
)


#
par(mfrow = c(2, 1))
par(cex = 2)
plot(c(y1, y2),
  type = "h",
  lwd = 8,
  xlab = "Time",
  ylab = "Cases"
)
lines(c(mu1, mu2), col = 2, lwd = 3)

par(cex = 2)

msg <- ifelse(
  test = fitdata$fail_to_reject_poisson,
  yes = "Retain Poisson",
  no = "Reject Poisson"
)

plot(surfdata$logtheta, surfdata$loglik,
  main = msg,
  type = "l"
)

par(cex = 1)
legend("topleft",
  legend = c(
    "Likelihood surface",
    "MLE from surface",
    "Estimate from LRT pipeline",
    "True value"
  ),
  col = c(1, 4, 2, 3),
  lty = c(1, 1, 3, 1),
  lwd = c(1, 3, 3, 2),
  bty = "n"
)

idx <- which.max(surfdata$loglik)
u <- surfdata$logtheta[idx]
segments(u, -10^6, u, 10^6, col = 4, lwd = 9)

v <- log(1 / fitdata$phi0)
segments(v, -10^6, v, 10^6, col = 2, lwd = 9, lty = 3)


w <- log(theta)
segments(w, -10^6, w, 10^6, col = 3, lwd = 2, lty = 1)
