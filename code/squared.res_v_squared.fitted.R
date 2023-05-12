# Check whether dispersion well represented by negative binomial model - just using the counties 
# with significant lrt result

source("code/W.R")
filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

signif_index <- c(1842, 1848, 1853, 1862, 1874, 1881, 1883, 1885, 323, 324, 329, 330, 364, 375)
state_fits <- c()

for (i in signif_index){
  state_fits <- c(state_fits, my_spl_fit(Y = new_cases_subset[i,], population = populations$population[i], 
                              inds = 1:60, df = 3))
}

state_fits <- matrix(state_fits, nrow = length(signif_index), ncol=7, byrow = TRUE)
state_fits <- cbind(state_fits, new_cases_subset[signif_index,])
state_fits <- as.data.frame(state_fits)
colnames(state_fits) <- c("kappa", "beta", "mu", "l", "j", "theta", "SE.theta")


# Plot squared residuals against squared fitted values
par(mfrow = c(2,4))
for (i in 1:8){   #NY
  plot(state_fits$mu[[i]]^2, (unlist(state_fits[i,8:67]) - state_fits$mu[[i]])^2, 
       col = 2, xlab = "Squared Fitted Values", ylab = "Squared Residuals")
  
}

par(mfrow = c(2,3))
for (i in 9:14){   #FL
  plot(state_fits$mu[[i]]^2, (unlist(state_fits[i,8:67]) - state_fits$mu[[i]])^2, 
       col = 2, xlab = "Squared Fitted Values", ylab = "Squared Residuals")
  
}

# If the constant dispersion model (NB2) model is OK, then we should see a linear trend. 
# If that checks out, then we don’t have to consider more complicated models.


