# Detecting changes in dispersion in COVID-19 incidence time series using a negative binomial model


## Introduction 

Time series of observed infectious disease incidence are, to varying degrees, "noisy", showing higher frequency oscillations around trends at broader temporal scales. Highly variable incidence that characterizes noisy data arise from imperfect and variable reporting (i.e, 'measurement error'), but also suggests transmission heterogeneity (superspreading), demographic/environmental heterogeneity, or changes in population effective reproduction number (R). Therefore, variability can contain information important to understanding epidemic dynamics and societal responses. Yet metrics of variability are often overlooked ways to understand these dynamics, and techniques based on variability in epidemic time series are still emerging. One area of interest is how variability is related to different phases of an epidemic. For instance, Graham et al. (2019) use the mean and interannual coefficient of variation of measles incidence to construct a metric indicative of where a location may be on the path to elimination of the pathogen. Similarly, it was recently found that $k_t$, the time-varying transmission heterogeneity for COVID-19, decreased over time and was significantly associated with interventions to slow spread in Hong Kong (Adam et al. 2022). Variability in *population-level* incidence time series may therefore provide information about what phase or dynamic regime an epidemic is in, as well as potentially indicating the level of heterogeneity at finer spatial and temporal scales, in transmission, susceptibility, reporting and/or that resulting from environmental/demographic stochasticity. Recently, an index of effective aggregate dispersion (EffDI) was proposed to elucidate clusters of infection from incidence data (Schneckenreither et al. 2023). Analyzing variability in terms of bursts of incidence is also important for planning surge capacity (Wallinga 2018). Sun et al. (2021) found a combination of individual-based and population-based strategies was required for SARS-CoV-2 control, further highlighting the importance of considering population-level variability and its relationship to individual-level variability. 

Dispersion around a rolling mean (moving window) of an incidence time series may contain information about the size and frequency of local outbreaks. From the perspective of an infectious individual, incidence experienced in their local spatial-temporal neighborhood will be higher than the global mean when dispersion is high. A 'mean crowding' parameter was proposed, which is the mean number per individual of other individuals in the same quadrat (Lloyd 1967). This is a useful way to think about dispersion in case count time series, as (absent other processes that influence dispersion such as variation in reporting) one can think of degree of dispersion as degree of clustering/crowding of cases, which is related to concepts like transmission heterogeneity. Specifically, individual-level heterogeneity in transmission scales up to affect population-level dynamics (Lloyd-Smith 2005), so dispersion in epidemic trajectories at the population level may provide information about individual-level variability in the transmission process. Individual-level variability in transmission is most often studied using contact-tracing data. However, contact tracing data requires intense investment of resources (Kretzschmar et al. 2020), so analysis of incidence data may often be more feasible.

One of the reasons why studying variability in incidence time series is not more widely done is because it is difficult to disentangle the effects of population size/incidence on variance. For instance, if we model a process as Poisson, we assume that if the mean of the process is large, the variance will be large as well. Since mean case counts is directly related to population size/incidence, we would infer that variance in case counts is large in large population/incidence settings. Furthermore, the rate of the case-count-generating process is often changing in an epidemic, which cannot be accomodated by a Poisson model. The negative binomial distribution may accurately model a time series if there is a changing process mean: for example, if the mean of a Poisson distribution itself follows a gamma distribution, the resulting distribution is negative binomial (Cook 2009). The result is that we should use the dispersion parameter of a negative binomial distribution to measure changes in meaningful variability in settings with differing base population/incidence, not the variance of a Poisson model. Negative binomial regression (in contrast to Poisson regression) can account for unobserved heterogeneity, time dependence in the rate of a process and contagion that all lead to overdispersion (Barron 1992).

We develop a method that quantifies the evolution of dispersion along incidence time series, allowing for the detection of changes in variability that are not due to changes in population size or overall burden of incidence. We apply the method to COVID-19 incidence data in US counties to investigate the relationships between incidence, dispersion and epidemic dynamic regimes over a portion of the pandemic. From a modern theoretical ecology perspective, investigating beyond the first moment of a process has also been identified as important: ecological experiments are typically geared towards assessing the impacts of the mean strength of causal processes, however the variance about mean effects have been mostly ignored as a driver in biological assemblages, but may be as important as the mean (Benedetti-Cecchi 2003).

## Results

We found that the negative binomial method is robust to changes in population size (for population sizes of at least 10,000). For an analytical derivation, see Supplement 1. The criteria are that the average p-value is 0.5 when the effect size is zero, and low average p-values are observed with increasing effect size. In row one and two of Figure 1, we illustrated that a drop in $\theta$ is associated with increased variability in simulated incidence time series, and that the same relationship is observable in the empirical time series, with an increase in $\theta$ corresponding to a decrease in variability around the trend in incidence.

<p align="center">
  <img src="figures/fig1.png" alt="Figure 1" title="Figure 1" />
</p>
<p align="center">Figure 1. Detecting dispersion changes in incidence time series in populations of different sizes. A) Simulated incidence when dispersion is constant and B) when dispersion changes during the epidemic. C) Daily COVID-19 cases in Jefferson County, AL and associated dispersion estimates. D) Shows the performance of the method with simulated data that has different absolute differences in theta (horizontal axis of each pane) across different population sizes (each pane is one population size). O and X mark the null and alternative hypotheses indicated in panels A and B. </p>

Highly overdispersed incidence patterns were observed more frequently later in time series, consistent with more heterogeneity in transmission, susceptibility and reporting. Interestingly, the most dispersed category reaches its highest proportion near the end of the timeframe (Figure 1.(a)) In addition, there are increases in dispersion around the holiday periods in the dataset (Figure 1.(b)), concurrent with increases in incidence (Figure 1.(c)). The evidence for a change in $\theta$ was observed across many counties (evidenced by concentration of low p-values concurrent with peak incidence) (Figure 1.(d)).

<p align="center">
  <img src="figures/roughdraft_surfaces.png" alt="Figure 2" title="Surfaces" />
</p>
<p align="center">Figure 2.  Evolution of dispersion between 2020-02-20 and 2023-03-19 in large counties in the US. a) Binned log of the dispersion parameter over time; b) Log of the dispersion parameter over time as well as for each of the large counties (y-axis); c) Log incidence (new cases per individual) over time as well as for each of the large counties (y-axis); d) LRT p-values over time as well as for each of the large counties (y-axis).</p>


The occurrence of high dispersion at times of peak incidence is of interest because it has more impact on variability than when incidence is lower. In other words, what makes a change in dispersion meaningful is how it affects variance and variance-mean relationships. For instance, if dispersion is high in a high incidence setting, the variance-mean ratio would be larger than for the same dispersion in a smaller incidence setting. A change from $\theta = 1000$ to $\theta = 100$ is operationally significant for large populations during times of peak incidence due to this variance-mean scaling:
$$var = \mu + \mu^2/\theta$$
So, a small increase in dispersion could have large impacts on the variance in large populations at times of peak incidence. Raising variance relative to mean implies spatiotemporal "crowding" of cases (i.e. localized surges) which may necessitate more surge capacity in hospitals and testing centers. Additionally, it may indicate less diffuse epidemics that are potentially more subject to climate forcing (Dalziel et al. 2018), or increased locally experienced mean density (Lloyd 1967). To illustrate this using the empirical data, we observed an increase from $\theta$ = 2.707735e+16 to $\theta = 21.8711$, which, conditional on the observed case count of 3$$, would lead to an increase in variance from around 3 to around 3.4. In contrast, if the observed case count were 300, variance would go from around 300 to around 4415.019.

<p align="center">
  <img src="figures/theta_cycles.png" alt="Figure 3" title="Loops" />
</p>
<p align="center">Figure 3.  .</p>

## Materials and Methods

### Introduce the method 
Classical theory put forward by Grenfell et al. (2002) proposed that incidence can be modeled by a negative binomial variable with expectation $\mu$ equal to the epidemic intensity and dispersion $\theta$ equal to previous incidence:
$$I_{t+1} = NB(\mu = \lambda_{t+1}, \theta_t = I_t)$$
However, other processes besides the current number infected might affect dispersion, so we instead investigated changes in $\theta_t$ to understand important processes that may leave a signal in dispersion. In other words, we estimated $\theta$ over time. 
A persistent challenge in investigating changes in variability has been "spurious correlation" with population size. Since population size influences mean and variance in count data and thus could have an impact on estimates of dispersion, we robustly adjusted for population size using an offset in the model. 
In sum, our method identifies shifts in population-level dispersion in incidence while accounting for population size. 
The general framework is that incidence is drawn from a negative binomial distribution with time-varying mean and dispersion parameters (that vary more slowly than the mean). 
The model is formulated as follows:
  1) The linear predictor includes a natural spline in time with three degrees of freedom to account for autocorrelation in case counts. Natural splines are cubic splines which are linear outside of the boundary knots (Perperoglou et al. 2019). A recently proposed negative binomial regression model for time series of counts also accommodates serial dependence (Davis and Wu 2009).
  b) There is an offset term in order to directly model counts (here, COVID-19 cases) per unit of observation (here, per individual):
  $$log(E[Y_i]/n_i) = \beta_1h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i)$$
  $$log(E[Y_i])-log(n_i) = \beta_1h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i)$$
  $$log(E[Y_i]) = \beta_1(h_1(t_i) + \beta_2h_2(t_i) + \beta_3h_3(t_i) + log(n_i)$$

So, the form of the probability mass function is:
$$f_t(I) = {I + \theta - 1 \choose I} \frac{\mu}{\mu+\theta}^I \frac{\theta}{\mu +\theta}^\theta$$
This form has expectation and variance as follows:
$$E(I) = \mu$$
$$Var(I) = \mu + \frac{\mu^2}{\theta}$$

### Application of the method to simulated data

For validity/power simulations, we used both Gaussian and uniform epidemic curves with an attack rate of 0.1 in the simulated set of time series--3,000 epidemic curves over 60 days each were produced, and the LRT test procedure was applied to each. Varying the effect size, location of the breakpoint, population size, and curve shape allowed us to test the validity and power of our approach.

### Application of the method to empirical data
To evaluate whether the negative binomial conditional distribution is needed (opposed to a Poisson/quasi-Poisson conditional distribution with the same model of process mean), inspection of mean-variance relationships using a diagnostic plot is possible (Ver Hoef and Boveng 2007), along with evaluation of dispersion statistics.
We then estimated $\mu_t$ and $\theta$ using iterative reweighted least-squares (procedure implemented via the NBPSeq R package (https://CRAN.R-project.org/package=NBPSeq) and from Di et al. (2011)) with a moving window approach. For each window, $\mu_t$ was estimated using a spline function in time, and a single value of $\theta$ was estimated for the window. By moving the window one time step at a time, a time series for $\theta_t$ was produced. We investigated large counties (top 4% in each state), due to the power constraints discussed above.

## Discussion

Here we presented an approach to comparing variability in epidemic time series that does not detect artifacts based on population size and incidence. Our method forms part of a larger push toward investigating variability as an important attribute of epidemic time series. The ability to compare variability both within one location over time and across locations is crucial to inform the timing and geographical allocation of public health resources. Methods that use incidence time series are a key part of this movement because of the ease of obtaining this data, so the aforementioned goal can be achieved with limited resrouces. 

Our results imply that we can revise our understanding of case count dispersion: dispersion is high at unexpected times (peak incidence) and corresponds to significant increases in variance when incidence is high. This flies in the face of the notion that large cities are always subject to more "smooth" epidemic dynamics. 

In addition, a big city with more hospitals could effectively be analogous to a collection of small towns, and therefore experience a benefit of reduced variance-mean relationships, especially during periods of peak incidence.
This implies that there may be merit to revising current public health strategies. 

Though some kinds of time dependence in the rate can cause autocorrelation, as can contagion (if it occurs outside of set periods), and heterogeneity (if an omitted variable is correlated in time) (Barron 1992), our focus on dispersion makes sense because we are concerned with the clustering of cases from the point of view of an individual case. Also, demographic structure (e.g., age structure) has the potential to affect temporal autocorrelation in transmission rate - the effects of age structure can be captured by a model that includes an infection rate that varies over time (Earn et al. 1998). 

## Supplement

1. Analytical approach: robust to population size changes





