# Map estimates from the Thanksgiving survey

rm(list = ls())
graphics.off()

library(magrittr)
library(ggplot2)
library(viridisLite)
library(urbnmapr)

load("data/W_pvals_and_thetas_allcounties.Rdata")
load("data/processed_dat.RData")


counties_sf <- get_urbn_map("counties", sf = TRUE)

# Merge data
x <- counties_sf
y <- cbind(populations, theta1, theta2, pvals)
x$county_fips <- as.numeric(x$county_fips)
z <- merge(x, y, by.y = "countyFIPS", by.x = "county_fips", all.x = TRUE)


z$dtheta <- z$theta1 - z$theta2
z$dtheta_trimmed <- z$dtheta
z$dtheta_trimmed[z$dtheta_trimmed > 10] <- 10
z$dtheta_trimmed[z$dtheta_trimmed < -10] <- -10

z$f <- cut(z$dtheta, breaks = c(-Inf, seq(-20, 20, 1), Inf))


#cols <- turbo(nlevels(z$f), begin = 0, end = 1)
pal <- c("darkred", 
         "grey", 
         "seagreen2")
ramp <- colorRampPalette(pal)
cols <- ramp(nlevels(z$f))


p <- z %>%
    ggplot() +
    geom_sf(mapping = aes(fill = dtheta_trimmed)) +
    coord_sf(datum = NA) 
    
    #+
    #scale_fill_manual(values = cols)
