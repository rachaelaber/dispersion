# Detecting changes in dispersion in COVID-19 incidence time series using a negative binomial model


## Introduction (why is dispersion important?)

1. Mean may not capture context-dependent changes in epidemic trajectories

2. Metrics of variability are often overlooked and useful ways to understand epidemic dynamics

2. Overdispersed incidence suggests overdispersed individual reproductive number (superspreading), demographic/environmental heterogeneity, changes in population effective reproduction number (R)
(Mention relationship to autocorrelation?)

4. So, measuring (changing) dispersion would:
 a) Give a more complete (predictive) understanding at both individual and population level (scales up)
 b) Allow for preparing surge capacity in certain months

5. We could use dispersion estimates to identify areas of space and periods of time that could benefit from targeted control strategies to reduce superspreading
 a) Could also evaluate the success of these strategies

## Introduce the method

1. Our method identifies shifts in population-level dispersion in incidence

2. NB regression can account for unobserved heterogeneity, time dependence in the rate of a process/contagion that all lead to overdispersion (Barron 1992)

3. We examined dispersion statistics, and diagnostic plots(s) to make sure we needed to account for overdispersion

3. The model components and why we include them:
  a) Linear predictor includes a natural spline (3 df) in time to account for autocorrelation in case counts
  b) Offset term in order to directly model counts (here, COVID-19 cases) per unit of observation (here, per individual)
  
  $$log(E[Y_i]/n_i) = \beta_1h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i)$$
  $$log(E[Y_i])-log(n_i) = \beta_1h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i)$$
  $$log(E[Y_i]) = \beta_1(h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i) + log(n_i)$$

4. IRLS to get parameter estimates of the model

## Application of the method to simulated data

1. Validity/power simulations: used Gaussian and uniform epidemic curves

2. Also simulated data assuming that theta was a function of the mean of the process in order to assess the robustness of the test (Supplement)

3. Include tabulated Type I error rate and power as well as p-value plot?

## Thanksgiving hypothesis

1. For every county, NB model was fit separately to either side of November 26th, 2020 (Thanksgiving 2020)

2. Values of $\hat{\theta_1}$ and $\hat{\theta_2}$ from either side of putative changepoint used in LRT
to test the hypothesis that dispersion changes 

3. US map (no consistent directional theta change)

### Scanning for breakpoints 

1. LRT statistic doesn't necessarily track with changes in mean incidence in a county, but rather captures dispersion changes

2. Investigated large counties (top 4% in each state)

2. Theta and p-value surfaces indicate that cases fluctuated more around a time-averaged county trajectory during N-F 



