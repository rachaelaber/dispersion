# Null hypothesis as an envelope

rm(list = ls())
graphics.off()


# Script parameters
use_simulated_data <- FALSE


# Load function to fit model
source("code/fit.nb.regression.R")
source("code/my_spl_fit.R")
source("code/lrt.R")




# Load data and set indices for example time series
if (use_simulated_data) {
  load("data/simulated_curves.Rdata")
  targets <- c(210, 35) # rejected and not rejected, respectively
  new_cases_subset <- curves
  populations_subset <- curve_parms
  filename <- "figures/envelope_sim.pdf"
} else {
  load("data/processed_dat.Rdata")
  targets <- c(100, 9)
  filename <- "figures/envelope.pdf"
}



# Function to make polygon
mypoly <- function(x, y1, y2) {
  xx <- c(x, rev(x))
  yy <- c(y1, rev(y2))
  polygon(xx, yy, col = "pink", border = NA)
}

# Create two-panel visualization
pdf(filename, width = 6, height = 6)
par(mfrow = c(1, 2))

for (target in targets) {
  cases <- new_cases_subset[target, ]
  pop <- populations_subset$population[target]
  p_value <- lrt(
    y1 = cases[1:30], y2 = cases[31:60],
    s1 = pop, s2 = pop, i1 = 1:30, i2 = 31:60, df1 = 3,
    df2 = 3
  )$p

  # Get the null process
  spl <- my_spl_fit(Y = cases, population = pop, inds = 1:60, df = 6)

  # Sample from the null process
  mu <- spl$mu
  theta <- spl$theta

  nsim <- 1000
  sim_matrix <- matrix(NA, nsim, 60)

  for (i in 1:nsim) {
    sim_matrix[i, ] <- rnbinom(n = 60, mu = mu, size = theta)
  }

  # Construct quantiles
  ll <- apply(sim_matrix, 2, quantile, probs = 0.025)
  ul <- apply(sim_matrix, 2, quantile, probs = 0.975)

  # Plot
  plot(cases, pch = 21, cex = 1, main = sprintf("LRT p-value:%.2f", p_value))
  # points(sim, col = 3, pch = 19, cex = 4)
  # lines(ll, col = 2)
  # lines(ul, col = 2)
  mypoly(1:60, ll, ul)
  lines(mu, col = 2, lwd = 2)
  pch <- ifelse(cases > ul | cases < ll, 19, 21)
  points(cases, cex = 1, pch = pch)
  segments(30, -10, 30, 10^6)
}

dev.off()
