library("NBPSeq");
library("gam")

##' Likelihood ratio test for constant dispersion 
##'
##' .. content for \details{} ..
##' 
##' @title 
##' @param y1, y2, reponse (counts) 
##' @param s1, s2, offset 
##' @param i1, i2, indices (e.g. days)
##' @param i1, i2, indices (e.g. days)
##' @param df1, df2, d.f. for the spline on either side
##' 
##' @return 
##' @author Yanming Di
lrt = function(y1, y2, s1, s2, i1, i2, df1, df2){
  
  ## Find spline basis
  x1 = ns(i1, df=df1, intercept=TRUE);
  x2 = ns(i2, df=df2, intercept=TRUE);
  
  ## Find preliminary estimates of mu assuming phi=1. Will serve as
  ## initial values for the later irls algorithm.
  mustart1 = irls.nb.1(y1, s1, x1, 1.0)$mu;
  mustart2 = irls.nb.1(y2, s2, x2, 1.0)$mu;
  
  ## Log likelihood of log(kappa) under H0: no changepoint in kappa
  l0 =  function(lkappa) {
    kappa = exp(lkappa);
    phi = 1/kappa;
    res1 = irls.nb.1(y1, s1, x1, phi, mustart = mustart1);
    res2 = irls.nb.1(y2, s2, x2, phi, mustart = mustart2);
    sum(dnbinom(y1, size = kappa, mu = res1$mu, log = TRUE)) + 
      sum(dnbinom(y2, size = kappa, mu = res2$mu, log = TRUE));
  }
  
  obj0 = optimize(l0, c(log(1e-6), log(1e6)), maximum=TRUE);
  kappa0 = exp(obj0$maximum);
  l0 = obj0$objective;
  phi0 = 1/kappa0;
  res01 = irls.nb.1(y1, s1, x1, phi0, mustart=mustart1);
  res02 = irls.nb.1(y2, s2, x2, phi0, mustart=mustart2);
  
  ## Log likelihood of log(kappa) under H0: no changepoint in kappa
  l1 =  function(lkappa) {
    kappa = exp(lkappa);
    phi = 1/kappa;
    res1 = irls.nb.1(y1, s1, x1, phi, mustart=mustart1);
    sum(dnbinom(y1, size=kappa, mu=res1$mu, log=TRUE));
  }
  
  obj1 = optimize(l1, c(log(1e-6), log(1e6)), maximum=TRUE);
  kappa1 = exp(obj1$maximum);
  l11 = obj1$objective;
  phi11 = 1/kappa1;
  res11 = irls.nb.1(y1, s1, x1, phi11, mustart=mustart1);
  
  l2 =  function(lkappa) {
    kappa = exp(lkappa);
    phi = 1/kappa;
    res2 = irls.nb.1(y2, s2, x2, phi, mustart=mustart2);
    sum(dnbinom(y2, size=kappa, mu=res2$mu, log=TRUE));
  }
  
  obj2 = optimize(l2, c(log(1e-6), log(1e6)), maximum=TRUE);
  kappa2 = exp(obj2$maximum);
  l12 = obj2$objective;
  phi12 = 1/kappa2;
  res12 = irls.nb.1(y2, s2, x2, phi12, mustart = mustart2);
  
  lambda = (l11 + l12 - l0) * 2;
  p = pchisq(lambda, df = 1, lower.tail = FALSE);
  
  list(p = p, lambda = lambda, l0 = l0, l11 = l11, l12 = l12, 
       phi0 = phi0, phi11 = phi11, phi12 = phi12, 
       res01 = res01, res02 = res02, res11 = res12, res12 =res12)
  
}
