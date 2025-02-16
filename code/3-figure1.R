rm(list = ls())
graphics.off()



# Parameters
df <- 3
ww <- 8
ptol <- 1e-4
ztol <- 1e4
nsim <- 2 * 10^3
poprange <- c(4, 7)   # range of population sizes, log10 scale
thetamag <- 2         # largest value of abs(log10(theta))


# Load data and functions
source("code/lrt.R")
source("code/dispersion_test.R")
load("data/processed/nyt_weekly.Rdata")



# Construct smoothed incidence series
# based on US national COVID-19 incidence
# to use for the underlying trends
cases_smooth <- smooth.spline(colSums(cases), df = 32)$y
incidence_smooth <- cases_smooth / sum(pops)


# Function to generate and fit a sample trajectory
sim <- function(theta1, theta2, s, a = NULL, b = NULL) {
  if (is.null(a) && is.null(b)) {
    u <- 25:(144 - 2 * ww)
    a <- sample(u, 1)
    b <- a + 2 * ww - 1
  }

  incidence_sample <- incidence_smooth[a:b]

  mu <- incidence_sample * s

  i1 <- 1:ww
  i2 <- (ww + 1):(2 * ww)

  mu1 <- mu[i1]
  mu2 <- mu[i2]

  y1 <- rnbinom(ww, size = theta1, mu = mu1)
  y2 <- rnbinom(ww, size = theta2, mu = mu2)

  y <- c(y1, y2)

  out <- dispersion_test(y, s, df, ptol, ztol) # nolint

  return(list(
    theta1_est = out$theta1_est,
    theta2_est = out$theta2_est,
    p = out$p,
    fail_to_reject_poisson = out$fail_to_reject_poisson,
    collapse_to_zero = out$collapse_to_zero,
    y = c(y1, y2),
    mu = c(mu1, mu2)
  ))

  return(out)
}



# Apply
simdata <- data.frame(
  s = 10^runif(nsim, poprange[1], poprange[2]),
  theta1 = 10^runif(nsim, -thetamag, thetamag),
  theta2 = 10^runif(nsim, -thetamag, thetamag)
)

i <- sample.int(nsim, ceiling(0.1 * nsim)) # set some theta2 = theta1
simdata$theta2[i] <- simdata$theta1[i]

x <- abs(log10(simdata$theta2 / simdata$theta1))
i <- x > 3 # rare very large differences
simdata$theta2[i] <- simdata$theta1[i] # set theta1 = theta2



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




# Plot
filename <- "ms-3/fig1.pdf"
pdf(filename)


par(mfrow = c(2, 2))


# panel a
par(pin = c(2, 2))
plot(incidence_smooth * 1000,
  type = "l",
  lwd = 3,
  xlab = "Week",
  ylab = "Cases per 1000",
)

mtext("a", side = 3, line = 1, adj = 0, cex = 1.2)

u <- 25:(144 - 2 * ww)
a <- 86
b <- a + 2 * ww - 1

lines(a:b, incidence_smooth[a:b] * 1000,
  lwd = 5,
  col = 2
)



# panel b
target <- 73
cases_snippet <- cases[target, a:b]

out <- dispersion_test( # nolint
  cases_snippet,
  pops[target],
  df,
  ptol,
  ztol
)

y <- c(out$res01$mu, out$res02$mu)

ylim <- range(c(cases_snippet, y)) * 1e-3
ylim[2] <- 1.5 * ylim[2]
ylim[1] <- 0

plot(a:b, cases_snippet * 1e-3,
  xlab = "Week",
  ylab = "Cases per 1000",
  ylim = ylim,
)

mtext("b", side = 3, line = 1, adj = 0, cex = 1.2)

lines(a:b, y * 1e-3, col = 2, lwd = 3)
segments(a + ww, -1e6, a + ww, 1e6)

theta1_est_rounded <- format(round(out$theta1_est, 1), nsmall = 1)
theta2_est_rounded <- format(round(out$theta2_est, 1), nsmall = 1)

text(
  a,
  0.95 * ylim[2],
  bquote(hat(theta)[1] == .(theta1_est_rounded)),
  pos = 4
)

text(
  b,
  0.95 * ylim[2],
  bquote(hat(theta)[2] == .(theta2_est_rounded)),
  pos = 2
)



# panel showing an example of a simulated trajectory
# not plotted
if (0) {
  out <- sim(
    theta1 = out$theta1_est,
    theta2 = out$theta2_est,
    s = pops[i],
    a = a,
    b = b
  )

  plot(a:b, out$y * 1e-3,
    ylim = ylim,
    xlab = "Week",
    ylab = "Cases per 1000",
  )
  lines(a:b, out$mu * 1e-3,
    col = 2,
    lwd = 5
  )

  segments(a + ww, -1e6, a + ww, 1e6, lty = 3, lwd = 3)

  text(
    a,
    0.95 * ylim[2],
    bquote(theta[1] == .(theta1_est_rounded)),
    pos = 4,
    cex = 1.5
  )

  text(
    b,
    0.95 * ylim[2],
    bquote(theta[2] == .(theta2_est_rounded)),
    pos = 2,
    cex = 1.5
  )
}


# panel c

col <- rep(1, nrow(simdata))
col[simdata$theta1_est > 1 / ptol] <- "purple"
col[simdata$theta1_est < 1 / ztol] <- "blue"



plot(
  x = log10(simdata$theta1),
  y = log10(simdata$theta1_est),
  xlab = expression(theta[1]),
  ylab = expression(hat(theta)[1]),
  col = col,
  xaxt = "n",
  yaxt = "n",
)
axis(1, at = c(-2, 0, 2), labels = expression(10^-2, 10^0, 10^2))
axis(2,
  at = c(-6, -3, 0, 3, 6),
  labels = expression(10^-6, 10^-3, 10^0, 10^3, 10^6)
)

mtext("c", side = 3, line = 1, adj = 0, cex = 1.2)

abline(0, 1, col = 2, lwd = 5)



# panel d
x <- abs(log10(simdata$theta2 / simdata$theta1))
y <- log2(simdata$p)


plot(
  x = x,
  y = y,
  xlab = expression(abs(log[10] ~ (theta[2] / theta[1]))),
  ylab = "p",
  xaxt = "n",
  yaxt = "n",
)
axis(1, at = c(0, 1, 2, 3, 4, 5))
axis(2,
  at = log2(c(0.5, 10^seq(-3, -12, -3))),
  labels = expression(2^-1, 10^-3, 10^-6, 10^-9, 10^-12)
)

mtext("d", side = 3, line = 1, adj = 0, cex = 1.2)


library(mgcv)
fit <- gam(y ~ s(x))
xp <- seq(0, 4, len = 100)
pred <- predict(fit, newdata = data.frame(x = xp), se.fit = TRUE)
yp <- pred$fit
yu <- yp + 3 * pred$se.fit
yl <- yp - 3 * pred$se.fit

polygon(c(xp, rev(xp)), c(yu, rev(yl)), col = rgb(1, 0, 0, 0.3), border = NA)

lines(xp, yp, col = 2, lwd = 5)
lines(xp, yu, col = 2, lwd = 1)
lines(xp, yl, col = 2, lwd = 1)

dev.off()
