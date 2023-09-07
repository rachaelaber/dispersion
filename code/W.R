library(splines)
source("code/nb_estimation.R")

my_spl_fit <- function(Y, population, inds, df) {
    y <- Y[inds]
    pop_vec <- rep(population, times = length(y))
    fit <- fit.nb.regression(y, pop_vec, ns(inds, df = df, intercept = TRUE))

    fit$theta <- fit$kappa
    fit$SE.theta <- sqrt(solve(fit$j)[1, 1])
    return(fit)
}


W <- function(y, population_size, breakpoint, deg_free = 3, fn = my_spl_fit, verbose = FALSE, 
              return_theta_diff = FALSE, return_theta = FALSE) {

    pop <- population_size
    df <- deg_free
    inds1 <- 1:breakpoint
    inds2 <- (breakpoint + 1):length(y)

    spline_mod.1 <- fn(Y = y, population = pop, inds = inds1, df = df)
    spline_mod.2 <- fn(Y = y, population = pop, inds = inds2, df = df)

    test_stat <- ((spline_mod.1$theta - spline_mod.2$theta) -
        0) / sqrt((spline_mod.1$SE.theta^2 +
        spline_mod.2$SE.theta^2))

    if (verbose == TRUE) {
        return(sprintf(
            "p-value: %.2f ; SE.theta1 = %.2f ; SE.theta2 = %.2f; theta1 =%.2f; theta2 = %.2f", 
            2 * (1 - pnorm(abs(test_stat))), spline_mod.1$SE.theta,
            spline_mod.2$SE.theta, spline_mod.1$theta, spline_mod.2$theta
        ))
    }

    theta1 = spline_mod.1$theta
    theta2 = spline_mod.2$theta
    p = 2 * (1 - pnorm(abs(test_stat)))

    if (return_theta_diff) {
        return(theta1 - theta2)
    } 
    if (return_theta) {
      return(theta2)
    }
    else {
        return(p)
    }
}
