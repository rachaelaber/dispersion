# Visualize fitted curves in a state's counties where significant Wald result observed

for (i in W_signif_index){
  
  mu <- my_spl_fit(Y = new_cases_subset[i,], population = populations$population[i], 
                   inds=1:length(new_cases_subset[i,]), df = 3)$mu
  
  plot(1:length(new_cases_subset[i,]), new_cases_subset[i,])
  lines(mu, col=2)
}