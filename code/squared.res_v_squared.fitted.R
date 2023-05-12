# Check whether dispersion well represented by negative binomial model 

source("code/W.R")
filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

state <- "NY"
state_index <- which(populations$State == state)
state_fits <- c()

for (i in state_index){
  state_fits <- c(state_fits, my_spl_fit(Y = new_cases_subset[i,], population = populations$population[i], 
                              inds = 1:60, df = 3))
}

state_fits <- matrix(state_fits, nrow = length(state_index), ncol=7, byrow = TRUE)
state_fits <- cbind(state_fits, new_cases_subset[state_index,])
state_fits <- as.data.frame(state_fits)
colnames(state_fits) <- c("kappa", "beta", "mu", "l", "j", "theta", "SE.theta")


# Plot squared residuals against squared fitted values
plot(state_fits$mu[[1]]^2, (unlist(state_fits[1,8:67]) - state_fits$mu[[1]])^2, xlab = 
       "Squared Fitted Values", ylab = "Squared Residuals")
for (i in 2:length(state_index)){
  points(state_fits$mu[[i]]^2, (unlist(state_fits[i,8:67]) - state_fits$mu[[i]])^2, 
         col = sample(rainbow(i)))
}
# If the constant dispersion model (NB2) model is OK, then we should see a linear trend. 
# If that checks out, then we don’t have to consider more complicated models.


