# Wrapper function for fit.nb.regression

my_spl_fit <- function(Y, population, inds, df) {
  y <- Y[inds]
  pop_vec <- rep(population, times = length(y))
  fit <- fit.nb.regression(y, pop_vec, ns(inds, df = df, intercept = TRUE))
  
  fit$theta <- fit$kappa
  fit$SE.theta <- sqrt(solve(fit$j)[1, 1])
  return(fit)
}