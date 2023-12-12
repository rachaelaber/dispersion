# Possible visualization for figure 1 (part 2), representing the null hypothesis as an envelope

rm(list = ls())
graphics.off()

# Load functions to fit model
source("code/W.R")

# Load simulated data and set indices for example time series
load("data/simulated_curves.Rdata")
targets <- c(220, 9) # rejected and not rejected, respectively


# Function to make polygon
mypoly <- function(x, y1, y2) {
  xx <- c(x, rev(x))
  yy <- c(y1, rev(y2))
  polygon(xx, yy, col = "pink", border = NA)
  
}

# Create two-panel visualization
filename <- "figures/envelope_sim.pdf"
pdf(filename, width = 6, height = 6)
par(mfrow = c(1, 2))

for (target in targets) {
  
  cases <- curves[target, ]
  pop <- curve_parms$population[target]
  w_p_value <- W(y = cases, population = pop, 
                 breakpoint = 30)
  
  # Get the null process
  spl1 <- my_spl_fit(cases, pop, inds = 1:30, df = 3)
  spl2 <- my_spl_fit(cases, pop, inds = 31:60, df = 3)
  
  # Sample from the null process
  mu <- c(spl1$mu, spl2$mu)
  theta <- spl1$theta
  
  nsim <- 1000
  sim_matrix <- matrix(NA, nsim, 60)
  
  for(i in 1:nsim) {
    sim_matrix[i, ] <- rnbinom(n = 60, mu = mu, size = theta)
  }
  
  # Construct quantiles
  ll <- apply(sim_matrix, 2, quantile, probs = 0.025)
  ul <- apply(sim_matrix, 2, quantile, probs = 0.975)
  
  # Plot
  plot(cases, pch = 21, cex = 1, main = sprintf("Wald test p-value:%.2f", w_p_value))
  #points(sim, col = 3, pch = 19, cex = 4)
  #lines(ll, col = 2)
  #lines(ul, col = 2)
  mypoly(1:60, ll, ul)
  lines(mu, col = 2, lwd = 2)
  pch <- ifelse(cases > ul | cases < ll, 19, 21)
  points(cases, cex = 1, pch = pch)
  segments(30, -10, 30, 10^6)
  
}

dev.off()

# load("./data/simulated_curves.Rdata")
# 
# filename <- "figures/empirical_increase.pdf"
# 
# pdf(filename, width = 6, height = 6)
# 
# american_thanksgiving <- as.Date("2020-11-26")
# 
# start_date <- american_thanksgiving - 29
# 
# end_date <- american_thanksgiving + 30
# 
# days <- seq(start_date, end_date, by = "day")
# 
# par(mfrow = c(2,1))
# 
# plot(curves[220, ] ~ days, xlab = "Date", ylim = c(0, 1400))
# abline(v = american_thanksgiving, col = 2)
# 
# plot(curves[212, ] ~ days, xlab = "Date", ylim = c(0, 1400))
# abline(v = american_thanksgiving, col = 2)
# 
# dev.off()
