# Visualize fitted curves in FL counties where significant LRT result observed

source("./code/W.R")

load("./data/processed_dat.Rdata")

load("./data/LRT_pvalsFL.Rdata")

FL_index <- which(populations_subset$State == "FL")

FL_index <- FL_index[(which(pvals < 0.05))]

filename <- "figures/FL_signif_fittedcurves.pdf"

pdf(filename, width = 6, height = 6)

par(mfrow = c(2, 3))

for (i in FL_index){
  
  mu <- my_spl_fit(Y = new_cases_subset[i,], population = populations_subset$population[i], 
                   inds = 1:length(new_cases_subset[i,]), df = 3)$mu
  
  plot(1:length(new_cases_subset[i,]), new_cases_subset[i,], xlab = , ylab = )
  lines(mu, col=2)
}

dev.off()
