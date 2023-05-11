library("MASS");
library("gam");
library("NBPSeq");

## install.packages("gam")


##' Find mle of the dispersion and the regression coefficients in a NB regression model
##'
##' .. content for \details{} ..
##' @title Find mle of the dispersion and the regression coefficients in a NB regression model
##' @param y 
##' @param s 
##' @param x 
##' @param beta0 
##' @param kappa 
##' @return 
##' @author Yanming Di
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


## Rachael's version

spl_fit <- function(Y, population, inds){ 
  pop_vec = rep(population, times=length(Y[inds]))
  mean_f = gam(Y[inds] ~ s(inds), family=poisson)
  m = mean_f$fitted.values
  fit = glm.nb(Y[inds] ~ m + offset(log(pop_vec)), link = log) 
  return(fit)
}

## Use glm.nb with ns function
## This version has convergence issues
glm_spl_fit <- function(Y, population, inds, df){ 
  y = Y[inds];
  pop_vec = rep(population, times=length(y));
  fit = glm.nb(y ~ ns(inds, df=df) + offset(log(pop_vec)), link = log) 
  return(fit)
}

## Use my irls.nb.1
## This version works, but it does not compute SE for theta
diy_spl_fit_v0 <- function(Y, population, inds, df){ 
  y = Y[inds];
  pop_vec = rep(population, times=length(y));
  
  lphi = function(logphi) {
     phi = exp(logphi);
     fit = irls.nb.1(y, pop_vec, ns(inds, df=df, intercept=TRUE), phi);
     sum(dnbinom(y, phi, mu=fit$mu, log=TRUE));
  }
  
  obj =  optimize(lphi, c(-10,10), maximum=TRUE);
  phi = exp(obj$maximum);
  fit = irls.nb.1(y, pop_vec, ns(inds, df=df, intercept=TRUE), phi = phi);
  fit$theta = phi;
  
  return(fit)
}

## Combine the above version with glm.nb
diy_glm_spl_fit <- function(Y, population, inds, df){ 
  y = Y[inds];
  pop_vec = rep(population, times=length(y));
  
  lphi = function(logphi) {
    phi = exp(logphi);
    fit = irls.nb.1(y, pop_vec, ns(inds, df=df, intercept=TRUE), phi);
    sum(dnbinom(y, phi, mu=fit$mu, log=TRUE));
  }
  
  obj =  optimize(lphi, c(-10,10), maximum=TRUE);
  phi = exp(obj$maximum);
  
  fit0 = irls.nb.1(y, pop_vec, ns(inds, df=df, intercept=TRUE), phi = phi);
  ## fit$theta = phi;
  
  fit = glm.nb(y ~ ns(inds, df=df) + offset(log(pop_vec)), mustart = fit0$mu, link = log) 
  
  
  return(fit)
}


## This version is similar to diy_spl_fit_v0, but also gives SE.theta
diy_spl_fit <- function(Y, population, inds, df){ 
  y = Y[inds];
  pop_vec = rep(population, times=length(y));
  fit = fit.nb.regression(y, pop_vec, ns(inds, df=df, intercept=TRUE));
  
  fit$theta = fit$kappa
  fit$SE.theta = sqrt(solve(fit$j)[1,1])
  return(fit)
}


sim.0 = function() {
  population = 1e6;
  day = 1:100;
  rate = exp(-(day-50)^2/100) * 0.1;
  y = rnbinom(100,  mu = rate * population, size = 10);
  
  plot(day, y)
  
  f1 = spl_fit(y, population, day);
  lines(day, f1$fitted.values)
  
  f2 = glm_spl_fit(y, population, day, df=5);
  lines(day, f2$fitted.values, col="magenta")
  
  fd = diy_spl_fit(y, population, day, df=5);
  lines(day, fd$mu, col="darkcyan");
  
  f2$theta
  f2$SE.theta
  fd$theta 
  fd$SE.theta 
 
  ## glm_spl_fit will often fail for higher df
  f9 = glm_spl_fit(y, population, day, df=9);
  lines(day, f9$fitted.values, col="darkcyan");
 
  ## diy_glm_spl_fit will not fix the issue
  fdg = diy_glm_spl_fit(y, population, day, df=9);
  lines(day, fdg$mu, col="darkcyan");
  
  fd = diy_spl_fit(y, population, day, df=9);
  lines(day, fd$mu, col="darkcyan");
  
  ## lines(day, f3$fitted.values, c=ol"cyan");
  ## lines(day, fd$mu, col="darkcyan");
  
  fit = irls.nb.1(y, population, ns(day, df=5, intercept=TRUE), phi = 1/theta0);
  lines(day, fit$mu, col="brown");
}
  
sim.1 = function() {
  
  set.seed(12345);
  
  theta0 = 10;
  population = 1e6;
  day = 1:100;
  rate = exp(-(day-50)^2/100) * 0.1;
  
  n.sim = 100;
  theta.glm = rep(NA, n.sim);
  theta.diy = rep(NA, n.sim);
  se.theta.glm = rep(NA, n.sim);
  se.theta.diy = rep(NA, n.sim);
  
  for (i in 1:n.sim) {
    print(i)
    y = rnbinom(100,  mu = rate * population, size = theta0);
    
    fd = try(diy_spl_fit(y, population, day, df=5));
    if (class(fd)!="try-error") {
      theta.diy[i] = fd$theta;
      se.theta.diy[i] = fd$SE.theta;
    }
    
    f5 = try(glm_spl_fit(y, population, day, df=5));
    if (class(f5)!="try-error") {
      theta.glm[i] = f5$theta;
      se.theta.glm[i] = f5$SE.theta;
    }
    
  }
  
  sum(is.na(theta.glm))
  sum(is.na(theta.diy))
  
  plot(theta.diy, theta.glm);
  plot(se.theta.diy, se.theta.glm);
  
  plot(theta.diy, theta.glm/theta.diy - 1)
  plot(se.theta.diy, se.theta.glm/se.theta.diy - 1)
  
  ## Check coverage
  sum((theta0> theta.diy - se.theta.diy * 2) & (theta0 < theta.diy + 2 * se.theta.diy))
  
  hist(theta.diy)
  
}
  