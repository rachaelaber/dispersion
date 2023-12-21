library(here)
library(sf)
library(maps)

load("data/lrt_pvals_phis_allcounties.Rdata")

load("data/processed_dat.Rdata")

county.map = map("county", fill = TRUE, plot = FALSE)
county.map = sf::st_as_sf(county.map) # Convert from map object to sf
plot(st_geometry(county.map)) # Only plot the polygons

ltheta1 <- log(1/phi11)
ltheta2 <- log(1/phi12)

# Prepare the dataframe to be merged w/polygon data
dat <- cbind(populations_subset, ltheta1, ltheta2, pvals)
dat$county <- gsub(" County", "", dat$County.Name)
dat$county <- gsub(" Parish", "", dat$county)
dat$county <- tolower(dat$county)
dat$state <- dat$State
dat$dtheta <- dat$ltheta2 - dat$ltheta1

## a clamped version of dtheta
clamp <- function(x, c) {
  x[x > c] <- c
  x[x < -c] <- -c
  return(x)
}
dat$dltheta_clamped <- clamp(dat$dtheta, 20)

data(county.fips)
county.map$FIPS = county.fips$fips[match(county.map$ID, county.fips$polyname)]
dat.clor = merge(county.map, dat, by.x = "FIPS", by.y = "countyFIPS", all.x = TRUE)

# Save map 

filename <- "./figures/map_diffltheta_US.pdf"

pdf(filename, width = 6, height = 6)

plot(dat.clor["dltheta_clamped"], main = "Change in log dispersion parameter - Thanksgiving 2020")

dev.off()
