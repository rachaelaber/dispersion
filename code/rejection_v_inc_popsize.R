# Using simulated data, visualize relationship between mean 
# p-value and population size, and between mean p-value and 
# final outbreak size

load("data/pvals_sim_LRT.Rdata")

load("data/simulated_curves.Rdata")

# Rejection (p-values) v. population size 
mean_pval <- tapply(pvals, curve_parms$population, mean)
plot(unique(curve_parms$population), mean_pval, type = "p", 
     ylim = c(0, 1))

# Rejection (p-values) v. final size
mean_pval2 <- tapply(pvals, curve_parms$final_size, mean)
plot(unique(curve_parms$final_size), mean_pval2, type = "p",
     ylim = c(0, 1))
