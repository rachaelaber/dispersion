library(here)
library(sf)
library(maps)

load("data/processed_dat.Rdata")

county.map = map("county", region = "florida", fill = TRUE, plot = FALSE)
county.map = sf::st_as_sf(county.map) # Convert from map object to sf

plot (st_geometry(county.map)) # Only plot the polygons

data(county.fips)
county.map$FIPS = county.fips$fips[match(county.map$ID, county.fips$polyname)]
dat.clor = merge(county.map, populations, by.x = "FIPS", by.y = "countyFIPS", all.x = TRUE)

plot(dat.clor$population, main = "Pop")
