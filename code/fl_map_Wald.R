# Create a map of p-values resulting from the Wald test on the FL data

library(tigris)
all_counties <- counties(state = NULL)

load("./data/W_pvals_and_thetas_allcounties.Rdata")
p_values <- data.frame(cbind(pvals, fips))

p <- round(pvals, digits=1)
p[is.na(p)] <- "na"
fl$p <- p

ggplot(fl) +
  geom_sf(aes(fill = p)) 
