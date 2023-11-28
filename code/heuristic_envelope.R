# Possible visualization for figure 1, representing the null hypothesis as an envelope

rm(list = ls())
graphics.off()

# Load functions to fit model
source("code/W.R")


# Load data and extract a case time series
load("data/processed_dat.Rdata")
target <- 1
cases <- new_cases_subset[target, ]
pop <- populations_subset$population[target]


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


# Function to make polygon
mypoly <- function(x, y1, y2) {
    xx <- c(x, rev(x))
    yy <- c(y1, rev(y2))
    polygon(xx, yy, col = "pink", border = NA)

}


# Plot
plot(cases, pch = 21, cex = 4)
#points(sim, col = 3, pch = 19, cex = 4)
#lines(ll, col = 2)
#lines(ul, col = 2)
mypoly(1:60, ll, ul)
pch <- ifelse(cases > ul | cases < ll, 19, 21)
points(cases, cex = 4, pch = pch)
segments(30, -10, 30, 10^6)