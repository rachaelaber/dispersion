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


W <- function(y, population_size, breakpoint, deg_free = 3, fn = my_spl_fit, verbose = FALSE) {
    
    spline_mod.1 <- fn(Y = y, population = population_size, inds = 1:breakpoint, df = deg_free)
    spline_mod.2 <- fn(Y = y, population = population_size, inds = ((breakpoint + 1):length(y)), df = deg_free)

    test_stat <- ((spline_mod.1$theta - spline_mod.2$theta) -
        0) / sqrt((spline_mod.1$SE.theta^2 +
        spline_mod.2$SE.theta^2))

    if (verbose == TRUE) {
        return(sprintf(
            "p-value: %.2f ; SE.theta1 = %.2f ; SE.theta2 = %.2f; theta1 =%.2f; theta2 = %.2f", 1 - pnorm(test_stat), spline_mod.1$SE.theta,
            spline_mod.2$SE.theta, spline_mod.1$theta, spline_mod.2$theta
        ))
    }

    return(2 * (1 - pnorm(abs(test_stat))))
}

