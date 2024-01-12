# Null hypothesis as an envelope
# Skip counties where spline model can't be fit

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
targets <- c()
unique_states <- unique(populations_subset$State)
nstate <- length(unique_states)

for (i in 1:nstate){
  
  j <- which(populations_subset$State == unique_states[i])
  q <- quantile(populations_subset$population[j], 0.96)
  k <- which(populations_subset$population[j] > q)
  
  targets <- c(targets, j[k])
}
ntarget <- length(targets)
theta1 <- rep(NA, ntarget)
theta2 <- rep(NA, ntarget)
p_value <- rep(NA, ntarget)


par(pin = c(6, 6), cex = 2.5)

j <- 0
for (target in targets) {
  j <- j + 1

  cases <- new_cases_subset[target, ]
  pop <- populations_subset$population[target]

  out <- tryCatch(lrt(
    y1 = cases[1:30], y2 = cases[31:60],
    s1 = pop, s2 = pop, i1 = 1:30, i2 = 31:60, df1 = 3,
    df2 = 3
  ), error = function(e) return(NA))
  
  if (all(is.na(out))) next
  
  p_value[j] <- out$p
  theta1[j] <- 1 / out$phi11
  theta2[j] <- 1 / out$phi12

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
  filename <- paste0("figures/large_counties/", target, ".pdf")
  pdf(file = filename, width = 6, height = 6)

  state <- populations_subset$State[target]
  county <- populations_subset$County.Name[target]
  pop <- populations_subset$population[target]
  title <- paste0(county, " ", state, ", ", ceiling(pop/1000), " thousand",
                  "\n p-value = ", round(p_value[j], 8))
  plot(cases / pop * 10^5, pch = 21, cex = 1, main = title, cex.main = 0.9)
  mypoly(1:60, ll / pop * 10^5, ul / pop * 10^5)
  lines(mu / pop * 10^5, col = 2, lwd = 2)
  pch <- ifelse(cases > ul | cases < ll, 19, 21)
  points(cases / pop * 10^5, cex = 1, pch = pch)
  segments(30, -10, 30, 10^6)

  dev.off()

  print(paste0(j, " out of ", ntarget))
}



#
plot(log(theta2), log(theta1),
     col = ifelse(p_value < 0.05, 2, 1),
     xlim = c(-5, 5),
     ylim = c(-5, 5),
     pch = 19)