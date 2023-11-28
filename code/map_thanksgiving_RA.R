library(here)
library(sf)
library(maps)

load("data/W_pvals_and_thetas_allcounties.Rdata")

load("data/processed_dat.Rdata")

county.map = map("county", fill = TRUE, plot = FALSE)
county.map = sf::st_as_sf(county.map) # Convert from map object to sf
plot(st_geometry(county.map)) # Only plot the polygons

# Prepare the dataframe to be merged w/polygon data
dat <- cbind(populations_subset, theta1, theta2, pvals)
dat$county <- gsub(" County", "", dat$County.Name)
dat$county <- gsub(" Parish", "", dat$county)
dat$county <- tolower(dat$county)
dat$state <- dat$State
dat$dtheta <- dat$theta2 - dat$theta1

## a clamped version of dtheta
clamp <- function(x, c) {
  x[x > c] <- c
  x[x < -c] <- -c
  return(x)
}
dat$dtheta_clamped <- clamp(dat$dtheta, 20)

data(county.fips)
county.map$FIPS = county.fips$fips[match(county.map$ID, county.fips$polyname)]
dat.clor = merge(county.map, dat, by.x = "FIPS", by.y = "countyFIPS", all.x = TRUE)

# Save map 

filename <- "./figures/map_difftheta_US.pdf"

pdf(filename, width = 6, height = 6)

plot(dat.clor["dtheta_clamped"], main = "Change in dispersion parameter - Thanksgiving 2020")

dev.off()

# # Spatial (Poisson) regression for dtheta_clamped
# 
# dat_clor = st_transform(dat.clor, crs = "ESRI:102004")
# nb = poly2nb(dat_clor)
# nb = nblag(nb, 2) #Create 2nd order and this makes nb into a list
# 
# #2nd-order has about twice as many links
# nb[[1]]
# nb[[2]] 
# 
# #Create adjacency matrix: 
# #Style B = binary for edges
# W = nb2listw(nb[[1]], style="B", zero.policy = TRUE)
# W2 = nb2listw(nb[[2]], style="B", zero.policy = TRUE) 
# 
# ## Standard linear regression
# Y = dat_clor$dtheta_clamped
# X = (dat_clor$population - mean(dat_clor$population, na.rm = TRUE)) #center (scale?**)
# fit0 = lm(Y ~ X)
# 
# ## Fit CAR model
# fit1.car = spautolm(Y ~ X, family = "CAR", listw = W)
# fit2.car = spautolm (Y ~ X, family = "CAR", listw = W2)
