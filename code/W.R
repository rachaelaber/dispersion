library(splines)
source("code/fit.nb.regression.R")

my_spl_fit <- function(Y, population, inds, df) {
    y <- Y[inds]
    pop_vec <- rep(population, times = length(y))
    fit <- fit.nb.regression(y, pop_vec, ns(inds, df = df, intercept = TRUE))

    fit$theta <- fit$kappa
    fit$SE.theta <- sqrt(solve(fit$j)[1, 1])
    return(fit)
}


W <- function(y, population_size, breakpoint, deg_free = 3, fn = my_spl_fit, 
              ret_ltheta_diff = FALSE, ret_ltheta = FALSE, ret_lthetas = FALSE) {

    pop = population_size
    df = deg_free
    inds1 = 1:breakpoint
    inds2 = (breakpoint + 1):length(y)
    indsall = 1:length(y)

    spline_mod.1 = fn(Y = y, population = pop, inds = inds1, df = df)
    spline_mod.2 = fn(Y = y, population = pop, inds = inds2, df = df)
    spline_mod.all = fn(Y = y, population = pop, inds = indsall, df = df)

    test_stat = ((log10(spline_mod.1$theta) - log10(spline_mod.2$theta)) -
        0)/sqrt(spline_mod.1$SE.theta^2/spline_mod.1$theta^2 +
        spline_mod.2$SE.theta^2/spline_mod.2$theta^2)

    theta1 = spline_mod.1$theta
    theta2 = spline_mod.2$theta
    p = 2 * (1 - pnorm(abs(test_stat)))

    if (ret_ltheta_diff) {
        return(log(theta1) - log(theta2))
    } 
    
    if (ret_ltheta) {
      return(log(spline_mod.all$theta))
    }
    
    if (ret_lthetas){
      return(list(c(log(theta1), log(theta2))))
    }
    
    else {
        return(p)
    }
}
