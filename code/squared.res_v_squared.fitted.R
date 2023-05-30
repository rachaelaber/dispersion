# Check whether dispersion well represented by negative binomial model - just using the NY and FL counties 
# with significant LRT result

source("code/W.R")

load("./data/processed_dat.Rdata")

load("./data/LRT_pvalsFL.Rdata")
FL_index <- which(populations$State == "FL")
FL_index <- FL_index[(which(pvals < 0.05))]

load("./data/LRT_pvalsNY.Rdata")
NY_index <- which(populations$State == "NY")
NY_index <- NY_index[(which(pvals < 0.05))]

signif_index <- c(NY_index, FL_index)
state_fits <- c()

for (i in signif_index){
  state_fits <- c(state_fits, my_spl_fit(Y = new_cases_subset[i,], population = populations$population[i], 
                              inds = 1:60, df = 3))
}

state_fits <- matrix(state_fits, nrow = length(signif_index), ncol = 7, byrow = TRUE)
state_fits <- cbind(state_fits, new_cases_subset[signif_index,])
state_fits <- as.data.frame(state_fits)
colnames(state_fits) <- c("kappa", "beta", "mu", "l", "j", "theta", "SE.theta")


# Plot squared residuals against squared fitted values
par(mfrow = c(2, 7))
for (i in 1:length(signif_index)){   
  plot(state_fits$mu[[i]]^2, (unlist(state_fits[i, 8:67]) - state_fits$mu[[i]])^2, 
       col = 2, xlab = "Squared Fitted Values", ylab = "Squared Residuals")
}

# If the constant dispersion model (NB2) model is OK, then we should see a linear trend. 
# If that checks out, then we don’t have to consider more complicated models.