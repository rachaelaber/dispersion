
Ben's rough notes on dispersion parameter in infectious disease models.

Underlying / general model. Infectious disease spreads through a branching process. [Based on these assumptions] the number of infected individuals at time $t$ will have a negative binomial distribution

$$
I_t \sim NB \left( \mu_t, \theta_t \right)
$$

where $\mu_t$ is the expected value for $I_t$ and $\theta_t$ is the dispersion parameter. This model is common in population biology (e.g. May's host-parisitoid work, Grenfell's papers, funk superspreading).Importantly smaller values of the dispersion parameter $\theta_t$ correspond to increasing amounts of overdispersion. Specifcally the dispersion parameter is related to the mean and variance as
$$
x
$$

An intepretation of the dispersion parameter is that events are $1 + \theta^{-1}$ times as crowded in time relative to a Poisson process with the same mean. 

When modeling infectious disease incidence the TSIR approach uses $\mu_t = \lambda I_{t-1}$ where $\lambda is the current local rate at which incidence at time t-1 produces new infections at time $t$ and $\theta_t = I_{t-1}$.

Why is $\theta_t = I_{t-1}$. This comes from the assumption that each of the individuals who acquired the infection at $t-1$ form ''independent lineages'' with identically distributed ''local'' rate parameter. Under these conditions $\theta_t = I_{t-1}$ is approximately right, at least for large populations, sufficiently large time step. (Kendall 1954, Bjorstand 2002).

Susceptible depletion in one lineage does not affect another, transmission rates are equal across lineages. But what if transmission is heterogeneous and susceptible depletion happens locally (so there is competition among lineages for susceptible individuals) or heterogeneously (some lineages have access to more susceptible than others). Reporting would also affect this because if $I_t$ observed is an underestimate then $\theta_t = I_{t-1}$ will be big relative to it, recognizing that the dispersion parameter in this framework should be based on number infected not number reported.

Instead of assuming $\theta_t = I_{t-1}$, let's learn about the relationship and what drives $\theta_t$, first by quantifying how it changes over time. (e.g., does it vary with $I_t$? that's the null hypothesis.) To do so we will also adopt a more general / phenominological model for $\mu_t$. Instead of $\mu_t = \lambda I_{t-1}$, which is a special kind of moving window, we will use a spline.

