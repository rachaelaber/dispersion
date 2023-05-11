# Divide a time series given a set of changepoints

get_segments <- function(series, changepoints){
  pts <- c(1,changepoints)
  segments <- rep(NA, times = length(pts)-1)
  for (i in 1:length(segments)){
    segments[i] <- list(series[pts[i]:(pts[i+1]-1)])
  }
  return(segments)
}

# Get (mean) pw stat. (could pass a known set of means) given list of vectors
get_mean_pw <- function(segments){  
  pws = rep(NA, length(segments))
  for (j in 1:length(segments)){
    segment = segments[[j]]
    n = length(segment)
    pws[j] = sum(segment^2)/mean(segment) - sum(segment)
  } 
  return(mean(pws, na.rm = TRUE))  
}

sim_sides <- function(parms, time){
  means <- u_t(n=parms$n, t=time, b=parms$b, c =parms$c)
  means1 <- means[1:(length(means)/2)]
  means2 <- means[(length(means)/2+1):length(means)]
  theta1 <- parms$theta1
  theta2 <- parms$theta2
  c.1 <- rnbinom(n=length(means1), mu=means1, size=theta1)
  c.2 <- rnbinom(n=length(means2), mu=means2, size=theta2)
  return(c(c.1, c.2))
}

spl_fit_pois <- function(Y, population, inds){ 
  y = Y[inds]
  pop_vec = rep(population, times=length(y))
  fit = glm(y ~ gam(Y[inds] ~ s(inds))$fitted.values + offset(log(pop_vec)), family=poisson(link=log))
  return(fit)
}

diy_spl_fit <- function(Y, population, inds, df){ 
  y = Y[inds];
  pop_vec = rep(population, times=length(y));
  fit = fit.nb.regression(y, pop_vec, ns(inds, df=df, intercept=TRUE));
  
  fit$theta = fit$kappa
  fit$SE.theta = sqrt(solve(fit$j)[1,1])
  return(fit)
}

W <- function(y, pop, split, deg_free=5, fn=diy_spl_fit, verbose = FALSE){
  spline_mod.1 <- fn(Y = y, population=pop, inds=1:split, df=deg_free)
  spline_mod.2 <- fn(Y = y, population=pop, inds=((split+1):length(y)), df=deg_free)
  test_stat <-((spline_mod.1$theta - spline_mod.2$theta)-
                 0)/sqrt((spline_mod.1$SE.theta ^2 +
                            spline_mod.2$SE.theta^ 2))
  if (verbose == TRUE){
    return(sprintf("p-value: %.2f ; SE.theta1 = %.2f ; SE.theta2 = %.2f; theta1 =%.2f; theta2 = %.2f", 1-pnorm(test_stat), spline_mod.1$SE.theta,
                   spline_mod.2$SE.theta, spline_mod.1$theta, spline_mod.2$theta))
  }
  return(2*(1-pnorm(abs(test_stat))))
}

u_t <- function(n, t, b, c){ # process mean
  (n/10)/sqrt(2 * pi * c) * exp(-(t-b)^2/(2*c))
}

bin_it <- function(fits, y, nbins = 4) {
  resid_df <- data.frame(cbind(fits, y))
  names(resid_df) <- c("fits", "y")
  resid_df <- resid_df[order(resid_df$fits), ]
  bw <- (max(resid_df$fits) - min(resid_df$fits)) / nbins
  
  bin_vars <-
    c(mean((resid_df$y[which(resid_df$fits < min(resid_df$fits) + bw)] -
              resid_df$fits[which(resid_df$fits < min(resid_df$fits) + bw)]) ^
             2))
  bin_means <-
    c(mean(resid_df$fits[which(resid_df$fits < min(resid_df$fits) + bw)]))
  for (i in 2:nbins) {
    bin_vars <-
      c(bin_vars, mean((resid_df$y[which(
        resid_df$fits > min(resid_df$fits) + (i - 1) * bw &
          resid_df$fits < min(resid_df$fits) + i *
          bw
      )] -
        resid_df$fits[which(
          resid_df$fits > min(resid_df$fits) + (i - 1) * bw &
            resid_df$fits < min(resid_df$fits) + i *
            bw
        )]) ^ 2))
    bin_means <-
      c(bin_means, mean(resid_df$fits[which(
        resid_df$fits > min(resid_df$fits) + (i - 1) * bw &
          resid_df$fits < min(resid_df$fits) + i * bw
      )]))
  }
  
  plot(bin_means,
       bin_vars,
       xlab = "Bin mean",
       ylab = "Bin average squared residual",
       col = 4)
}

fit.nb.regression = function(y, s, x, beta0=rep(NA, dim(x)[2]), kappa=NA) {
  
  ## Find preliminary estiamtes of mu assuming phi=1. Will serve as
  ## initial values for the later irls algorithm.
  mustart = irls.nb.1(y, s, x, 1.0, beta0)$mu;
  
  if (is.na(kappa)) {
    
    ## Log likelihood of log(kappa)
    ll =  function(lkappa) {
      kappa = exp(lkappa);
      res = irls.nb.1(y, s, x, 1/kappa, beta0, mustart);
      sum(dnbinom(y, size =kappa, mu=res$mu, log=TRUE)); 
      ## log.likelihood.nb(kappa, res$mu, y);
    }
    
    res = optimize(ll, c(log(1e-20), log(1e20)), maximum=TRUE);
    kappa.hat = exp(res$maximum);
  } else {
    kappa.hat = kappa
  }
  
  phi.hat = 1/kappa.hat;
  
  res = irls.nb.1(y, s, x, phi.hat, beta0, mustart);
  beta.hat = res$beta;
  
  mu.hat = res$mu;
  l.hat =  sum(dnbinom(y, size =kappa.hat, mu=mu.hat, log=TRUE));  
  ## l.hat =  log.likelihood.nb(kappa.hat, mu.hat, y);
  
  v.hat = drop(mu.hat + phi.hat * mu.hat^2);
  n = length(y);
  n.pars = length(beta0) + 1;
  
  j.hat = matrix(NA, n.pars, n.pars);
  j.hat[-1,-1] = t(x) %*% diag(mu.hat^2 * (y/mu.hat^2 - (y+kappa.hat)/(mu.hat+kappa.hat)^2) - (y-mu.hat)*mu.hat/v.hat) %*% x;
  ## Be careful with the sign!
  j.hat[1,-1] = j.hat[-1,1] = - matrix((y - mu.hat) * mu.hat / (mu.hat+kappa.hat)^2, 1, n) %*% x;
  j.hat[1, 1] = -sum(trigamma(kappa.hat+y) - trigamma(kappa.hat) + 1/kappa.hat - 2/(kappa.hat+mu.hat) + (y+kappa.hat)/(mu.hat + kappa.hat)^2);
  
  ##  i.hat = t(x) %*% diag(mu.hat^2/v.hat) %*% x;
  ## l = log.likelihood.nb(kappa.hat, mu = mu.hat, y);
  
  ## See whether Poisson model fits better
  ## l.poisson  = sum(dpois(y, lambda = mu.hat, log=TRUE));
  ## eps = 1e-7;
  ## if (l.poisson + eps >= l & kappa.hat > 1e8) {
  ## l =  l.poisson;
  ##   kappa.hat = Inf;
  ##}
  ## l = sum(dnbinom(y, size = 1/phi, mu=res$mu, log=TRUE)); 
  
  list(kappa = kappa.hat, beta = beta.hat, mu = mu.hat, l=l.hat, j=j.hat);
}

# spl_fit_old <- function(Y, population, inds){ 
#   pop_vec = rep(population, times=length(Y[inds]))
#   mean_f = gam(Y[inds] ~ s(inds), family=poisson)
#   m = mean_f$fitted.values
#   fit = glm.nb(Y[inds] ~ m + offset(log(pop_vec)), link = log) 
#   return(fit)
# }