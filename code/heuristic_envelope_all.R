# Null hypothesis as an envelope
# Skip counties where spline model can't be fit. Note that this
# is more stringent than the later requirement that spline model
# of either side must converge

rm(list = ls())
graphics.off()



# Load function to fit model
source("code/fit.nb.regression.R")
source("code/my_spl_fit.R")
source("code/lrt.R")




# Load data
load("data/processed_dat.Rdata")





# Function to make polygon
mypoly <- function(x, y1, y2) {
  xx <- c(x, rev(x))
  yy <- c(y1, rev(y2))
  polygon(xx, yy, col = "pink", border = NA)
}

# Plot
targets <- 1:200

par(pin = c(6, 6), cex = 2.5)

for (target in targets) {

  cases <- new_cases_subset[target, ]
  pop <- populations_subset$population[target]

  out <- tryCatch(lrt(
    y1 = cases[1:30], y2 = cases[31:60],
    s1 = pop, s2 = pop, i1 = 1:30, i2 = 31:60, df1 = 3,
    df2 = 3
  ), error = function(e) return(NA))
  
  if (all(is.na(out))) next
  
  p_value <- out$p
  theta1 <- 1 / out$phi11
  theta2 <- 1 / out$phi12

  # Get the null process
  spl <- tryCatch(my_spl_fit(Y = cases, population = pop, inds = 1:60, df = 6),
                  error = function(e) return(NA))

  if (all(is.na(spl))) next
  
  # Sample from the null process
  mu <- spl$mu
  theta <- spl$theta

  nsim <- 1000
  sim_matrix <- matrix(NA, nsim, 60)

  for (i in 1:nsim) {
    sim_matrix[i, ] <- rnbinom(n = 60, mu = mu, size = theta)
  }

  # Construct quantiles
  ll <- apply(sim_matrix, 2, quantile, probs = 0.1)
  ul <- apply(sim_matrix, 2, quantile, probs = 0.9)

  # Plot
  filename <- paste0("figures/", target, ".pdf")
  pdf(file = filename, width = 6, height = 6)

  state <- populations_subset$State[target]
  county <- populations_subset$County.Name[target]
  pop <- populations_subset$population[target]
  title <- paste0(county, " ", state, ", ", ceiling(pop/1000), " thousand",
                  "\n p-value = ", round(p_value, 8))
  plot(cases / pop * 10^5, pch = 21, cex = 1, main = title, cex.main = 0.9)
  mypoly(1:60, ll / pop * 10^5, ul / pop * 10^5)
  lines(mu / pop * 10^5, col = 2, lwd = 2)
  pch <- ifelse(cases > ul | cases < ll, 19, 21)
  points(cases / pop * 10^5, cex = 1, pch = pch)
  segments(30, -10, 30, 10^6)

  dev.off()

}

