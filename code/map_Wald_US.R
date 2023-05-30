# Create a map of p-values resulting from the Wald test on the FL data

library(tigris)
library(tidyverse)

all_counties <- counties(state = NULL)

# Remove leading zeroes from GEOID column
all_counties$GEOID <- as.character(as.numeric(all_counties$GEOID)) 

load("./data/W_pvals_and_thetas_allcounties.Rdata")
pvals <- round(pvals, digits = 1)
pvals[is.na(pvals)] <- "na"

p_values <- data.frame(cbind(pvals, fips))
p_values$fips <- as.character(p_values$fips)

map_df <- merge(all_counties, p_values, by.x = "GEOID", by.y = "fips",
                all.x = TRUE)

filename <- "figures/map_Wald_US.pdf"
pdf(filename, width = 6, height = 6)

ggplot(map_df) +
  geom_sf(aes(fill = pvals)) 

dev.off()
