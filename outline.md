# Detecting changes in dispersion in COVID-19 incidence time series using a negative binomial model


## Introduction (why is dispersion important?)

1. Metrics of variability are often overlooked and useful ways to understand epidemic dynamics.   However, techniques based on variability in epidemic time series' are emerging. One area of interest is how variability is related to different phases of an epidemic. For instance, Graham et al. (2019) use the mean and interannual coefficient of variation of measles incidence to construct a metric indicative of where a location may be on the path to elimination of the pathogen. Similarly, it was recently found that $k_t$, the time-varying transmission heterogeneity for COVID-19, decreased over time and was significantly associated with interventions to slow spread in Hong Kong (Adam et al. 2022).


2. Dispersion around the mean (moving window) contains information about the size and frequency of local outbreaks, which can in turn indicate locally experienced epidemic dynamics. 
Epidemic burstiness can also be studied from the perspective of the individual. This is a useful way to think about dispersion in case count time series, as one can think of degree of dispersion as degree of clustering/crowding of cases. A 'mean crowding' parameter was proposed, which is the mean number per individual of other individuals in the same quadrat (Lloyd 1967).  An added benefit of this framework is variation in incidence and population size might appear to influence theta if they are not properly adjusted for, and this approach allows us to sidestep that. 

2. Overdispersed incidence suggests overdispersed individual reproductive number (superspreading), demographic/environmental heterogeneity, changes in population effective reproduction number (R)
Individual-level heterogeneity in transmission scales up to affect population-level dynamics (Lloyd-Smith 2005), so variability in epidemic trajectories at the population level may provide information about individual-level variability in the transmission process. Importantly, contact tracing data is expensive to collect (cite), so analysis of incidence data may often be more feasible.
Comparison of simulated and observed incidence time series has been used to estimate the role of environmental and demographic stochasticity in measles: across community sizes, demographic stochasticity of measles becomes more important in small human populations, where dynamics can’t be described as well simply by contact rate and birth rate (Grenfell et al. 2002). A similar phenomenon may be observed at the end of an epidemic (cite). 

4. So, measuring (changing) dispersion would:
 a) Give a more complete (predictive) understanding at both individual and population level (scales up)
 Analyzing variability in epidemic dynamics in terms of bursts of incidence is also important for planning surge capacity (Wallinga 2018).
 b) Allow for preparing surge capacity in certain months
 Sun et al. (2021) found a combination of individual-based and population-based strategies was required for SARS-CoV-2 control, further highlighting the importance of considering population-level variability and its relationship to individual-level variability. 



 Note that some kinds of time dependence in the rate can cause autocorrelation, as can contagion (if it occurs outside of set periods), and heterogeneity (if an omitted variable is correlated in time) (Barron 1992). Also, demographic structure (e.g., age structure) has the potential to affect temporal autocorrelation in transmission rate - the effects of age structure can be captured by a model that includes an infection rate that varies over time (Earn et al. 1998). 

 
 
### Model
Potential eqn flow:

"Classical theory" (May and others host-parasitoid models then Grenfell et al.) (cite) says:
$$I_t(t+1) = NB(R_t I_t, I_t)$$

Other processes might affect size parameter, so maybe it isn't $I_t$, it's $\theta_t$. 

Would be interesting to look at changes in $\theta_t$ to understand important processes that may leave a signal in dispersion. Specifically estimate theta over time and scan for changepoints. 

### Telegraph main results?


## Results

1. Method is robust to changes in number of cases. Algebra and simulations.  
[compare.pdf] 
2. Highly overdispersed incidence patterns occurring more frequently later in time series, consistent with more heterogeneity in transmission, susceptibility and reporting
[roughdraft_surface.pdf]
3. Increases in dispersion around the holiday periods in the dataset; concurrent with increases in incidence.
[roughdraft_surface.pdf]
4. Evidence for a change in theta observed across many counties (evidenced by concentration of low p-values concurrent with peak incidence).
[roughdraft_surface.pdf]


A change from $\theta = 1000$ to $\theta = 100$ is operationally significant for large populations during times of peak incidence due to variance-mean scaling. In particular, for a period of high incidence, the variance/mean ratio will be larger than the variance/mean ratio for smaller incidence (assuming theta is the same in both populations). So, a small decrement in theta could have large impacts on the variance in large populations at times of peak incidence.

$$var = mu + mu^2/theta$$


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

## Discussion

The results imply that we can revise our understanding of case count dispersion: dispersion is high at unexpected times (peak incidence) and corresponds to significant increases in variance when incidence is high.

## Figures for Supplement

1. Simulations reveal method is robust to population size
[sim_pval_v_pop.pdf] 


Abstract:

Metrics of variability are often overlooked and useful ways to understand epidemic dynamics. For instance, superspreading of viruses such as SARS-CoV-2 can be elucidated by utilizing such metrics. Our method identifies shifts in population-level incidence dispersion, allowing a more complete and predictive understanding at both the individual and population level, and allowing practitioners to prepare surge capacity in certain months. Although classical theory predicts that there will be less dispersion when incidence is higher, we consider a more general negative binomial regression framework to take into account processes that may also affect the spread of cases. We investigate changes in dispersion over time and space and find that there are increases in dispersion around holiday periods in many US counties, concurrent with observed incidence increases. In addition, highly overdispersed patterns occur more frequently later in time series, consistent with more heterogeneity in transmission, susceptibility, and reporting. Our method is robust to changes in incidence and to population size, allowing for quantification of dispersion—indicative of superspreading dynamics—without artifactual contributions from these features.

-second sentence "our method" feels a bit out of place. Start by framing the problem. Maybe something about how, variability itself varies and affects epidemic dynamics is not well understood? [that may not be good.. just one possible starting point to take or leave]
-"we consider a more general negative binomial regression framework to take into account processes that may also affect the spread of cases" doesn't give a clear enough idea about the contribution. Something like: "We develop a flexible way of estimating how variability changes over time, including detecting breakpoints representing discrete shifts in variability." And it factors out population size and case numbers. Able to detect changes in variability that matter, and distinguish them from changes due to simple statistical effects of population size or case number.
-










Sources for methods:

The negative binomial distribution might accurately model a time series if there is a changing process mean: for example, if the mean of a Poisson distribution itself follows a gamma distribution, the resulting distribution is negative binomial (Cook 2009).

Negative binomial regression (in contrast to Poisson regression) can account for unobserved heterogeneity, time dependence in the rate of a process and contagion that all lead to overdispersion (Barron 1992).

A recently proposed negative binomial regression model for time series of counts also accommodates serial dependence (Davis and Wu 2009).

Natural splines are cubic splines which are linear outside of the boundary knots (Perperoglou et al. 2019).

 Therefore, to evaluate whether the negative binomial conditional distribution is needed (opposed to a Poisson/quasi-Poisson conditional distribution with the same model of process mean), inspection of mean-variance relationships using a diagnostic plot is possible (Ver Hoef and Boveng 2007)
 
 This procedure is implemented via the NBPSeq R package (https://CRAN.R-project.org/package=NBPSeq) and from Di et al. (2011). 

 From a modern theoretical ecology perspective, investigating beyond the first moment of a process has also been identified as important: ecological experiments are typically geared towards assessing the impacts of the mean strength of causal processes, however the variance about mean effects have been mostly ignored as a driver in biological assemblages, but may be as important as the mean (Benedetti-Cecchi 2003).

















