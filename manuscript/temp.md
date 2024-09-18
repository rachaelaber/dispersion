
#### Ben's rough notes on dispersion parameter in infectious disease models.

Infectious disease transmission follows a branching process, as new infections result from exposure to individuals who are currently infectious. Under a wide range of configurations for a branching process model, including the vast majority of parsimoneous ones, the number of infected individuals $I_t$ at time $t$ will have a negative binomial distribution (Kendall 1954)

$$
I_t \sim NB \left( \mu_t, \theta_t \right)
$$

where $\mu_t$ is the expected value for $I_t$ and $\theta_t$ is the dispersion parameter, which is related to the variance by $\mathrm{Var}[I_t] = \mu_t + \mu_t^2 / \theta_t$. Importantly smaller values of the dispersion parameter $\theta_t$ correspond to increasing amounts of dispersion, increasing amounts by which the variance in realized number infected $I_t$ exceeds the expected value, $\mu_t$. Conversely, the distribution of $I_t$ tends to a Poisson distribution (where variance equals mean) as $\theta_t$ becomes large. This model is common in population biology (e.g. May's host-parisitoid work, Grenfell's papers, funk superspreading).

An intepretation of the dispersion parameter for a time series model of counts is that events are $1 + \theta^{-1}$ times as "crowded" in time relative to a Poisson process with the same mean \cite{lloyd_mean_1967} (see appendix). For example $\theta = 1$ corresponds to a situation where the average number of infections in the same timestep as a randomly selected case will exceed the Poisson expectation by a factor of 2. In a simple example relavent to surge capacity in the health care system, $\theta = 1$ implies that a random infectious individual visiting the emergency department at a hospital would find it on average to be twice as crowded with other infectious individuals with the same pathogen than expected for a Poisson process with the same incidence rate.

That covers the interpretation and it is clear that a lot of modeling epidemic dynamics comes down to modeling $\mu_t$ and $\theta_t$. So how do we model $\mu_t$ and $\theta_t$? A time-series approximation to compartment epidemic models uses $\mu_t = \lambda I_{t-1}$ where $\lambda$ is the current local rate at which incidence at time t-1 produces new infections at time $t$ and $\theta_t = I_{t-1}$. Why is $\theta_t = I_{t-1}$? This comes from the assumption that each of the individuals who acquired the infection at $t-1$ form ''independent lineages'' with identically distributed ''local'' rate parameter. Under these conditions $\theta_t = I_{t-1}$ is right for sufficiently large populations. (Kendall 1954, Bjorstand 2002). But this model assumes that supopulations of infectious individuals eminating from one infected individuals are independent and statistically identical to those from another. That is, it assumes susceptible depletion in one lineage does not affect another, transmission rates are equal across lineages, and reporting rates do not vary across lineages. 

Since we know that is probably not true, we can use estimates of $\theta_t$ to understand how heterogeneity in transmission, susceptibility and reporting (and other factors) are driving epidemic dynamics. Instead of assuming $\theta_t = I_{t-1}$, let's learn about the relationship and what drives $\theta_t$, first by quantifying how it changes over time. (e.g., does it vary with $I_t$? that's the null hypothesis.) To do so we will also adopt a more general / phenominological model for $\mu_t$. Instead of $\mu_t = \lambda I_{t-1}$, which is a special kind of moving window, we will use a spline which can approximate the solution to a wider range of autoregressive and other models.



