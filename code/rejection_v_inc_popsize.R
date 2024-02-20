# Using simulated data, visualize relationship between mean 
# p-value and population size

load("data/pvals_sim_LRT.Rdata")

load("data/simulated_curves.Rdata")

# p-values v. population size 

mean_pval <- tapply(pvals, curve_parms$population, mean)

var_pval <- tapply(pvals, curve_parms$population, var)

se_pval <- sqrt(var_pval)

filename <- "figures/sim_meanp_v_pop.pdf"

pdf(file = filename, width = 6, height = 6)

plot(unique(curve_parms$population), mean_pval, type = "l", 
     ylim = c(0, 1), main = "Mean p-value v. population size",
     xlab = "Population Size", ylab = "Mean p-value")
lines(unique(curve_parms$population), (mean_pval + .3 * se_pval), col = 2)
lines(unique(curve_parms$population), (mean_pval - .3 * se_pval), col = 2)

dev.off()