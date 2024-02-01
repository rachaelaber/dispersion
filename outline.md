# Detecting changes in dispersion in COVID-19 incidence time series using a negative binomial model


## Introduction (why is dispersion important?)

1. Metrics of variability are often overlooked and useful ways to understand epidemic dynamics

2. Dispersion around the mean (moving window) contain information about the size and frequency of local outbreaks, which can in turn indicate X.

2. Overdispersed incidence suggests overdispersed individual reproductive number (superspreading), demographic/environmental heterogeneity, changes in population effective reproduction number (R)
(Mention relationship to autocorrelation?)
(Mention that variation in incidence and population size might appear to influence theta if they are not properly adjust for)

4. So, measuring (changing) dispersion would:
 a) Give a more complete (predictive) understanding at both individual and population level (scales up)
 b) Allow for preparing surge capacity in certain months



### Model
Potential eqn flow:

"Classical theory" (May and others host-parasitoid models then Grenfell et al.) (cite) says:
$$I_t(t+1) = NB(R_t I_t, I_t)$$

Other processes might affect size parameter, so maybe it isn't $I_t$, it's $\theta_t$. 

Would be interesting to look at changes in $\theta_t$ to understand important processes that may leave a signal in dispersion. Specifically estimate theta over time and scan for changepoints. 

### Telegraph main results?


## Results

1. Simulations reveal method is robust to changes in incidence and to population size
[envelopes] 
2. Increases in dispersion around the holiday periods in the dataset; concurrent with increases in incidence.
[roughdraft_surface.pdf]
3. Evidence for a change in theta observed across many counties (evidenced by concentration of low p-values)  
[roughdraft_surface.pdf]
4. Highly overdispersed incidence patterns occuring more frequently later in time series, consistant with more heterogeneity in transmission, susceptibility and reporting
[roughdraft_surface.pdf]




## Materials and Methods

### Introduce the method

Challenge has been "spurious correlation" with population size.
Leads into talking about model w offset.

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

### Application of the method to simulated data

1. Validity/power simulations: used Gaussian and uniform epidemic curves

2. Also simulated data assuming that theta was a function of the mean of the process in order to assess the robustness of the test (Supplement)

3. Include tabulated Type I error rate and power as well as p-value plot?

### Scanning for breakpoints 

1. LRT statistic doesn't necessarily track with changes in mean incidence in a county, but rather captures dispersion changes

2. Investigated large counties (top 4% in each state)

2. Theta and p-value surfaces indicate that cases fluctuated more around a time-averaged county trajectory during N-F 


Abstract:

Metrics of variability are often overlooked and useful ways to understand epidemic dynamics. For instance, superspreading of viruses such as SARS-CoV-2 can be elucidated by utilizing such metrics. Our method identifies shifts in population-level incidence dispersion, allowing a more complete and predictive understanding at both the individual and population level, and allowing practitioners to prepare surge capacity in certain months. Although classical theory predicts that there will be less dispersion when incidence is higher, we consider a more general negative binomial regression framework to take into account processes that may also affect the spread of cases. We investigate changes in dispersion over time and space and find that there are increases in dispersion around holiday periods in many US counties, concurrent with observed incidence increases. In addition, highly overdispersed patterns occur more frequently later in time series, consistent with more heterogeneity in transmission, susceptibility, and reporting. Our method is robust to changes in incidence and to population size, allowing for quantification of dispersion—indicative of superspreading dynamics—without artifactual contributions from these features.

































