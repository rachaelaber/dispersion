# Visualize fitted curves in NY and FL counties where significant LRT result observed

source("./code/W.R")
load("./data/processed_dat.Rdata")

load("./data/LRT_pvalsFL.Rdata")
FL_index <- which(populations$State == "FL")
FL_index <- FL_index[(which(pvals < 0.05))]

load("./data/LRT_pvalsNY.Rdata")
NY_index <- which(populations$State == "NY")
NY_index <- NY_index[(which(pvals < 0.05))]

signif_index <- c(NY_index, FL_index)

par(mfrow = c(2, 7))
for (i in signif_index){
  
  mu <- my_spl_fit(Y = new_cases_subset[i,], population = populations$population[i], 
                   inds=1:length(new_cases_subset[i,]), df = 3)$mu
  
  plot(1:length(new_cases_subset[i,]), new_cases_subset[i,])
  lines(mu, col=2)
}
