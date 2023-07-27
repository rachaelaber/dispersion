# Map estimates from the Thanksgiving survey

rm(list = ls())
graphics.off()

library(maps)
library(mapproj)
library(ggplot2)
library(viridis)
library(magrittr) 

# This from
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
data(unemp, package = "viridis")

county_df <- map_data("county", projection = "albers", parameters = c(39, 45))
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

state_df <- map_data("state", projection = "albers", parameters = c(39, 45))

choropleth <- merge(county_df, unemp, by = c("state", "county"), all.x = TRUE)
choropleth <- choropleth[order(choropleth$order), ]

p1 <- ggplot(choropleth, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = rate), colour = alpha("white", 1 / 2), linewidth = 0.2) +
    geom_polygon(data = state_df, colour = "white", fill = NA) +
    coord_fixed() +
    theme_minimal() +
    ggtitle("US unemployment rate by county") +
    theme(
        axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()
    ) +
    scale_fill_viridis(option = "magma")


# Adapt the above to plot Thanksgiving results
load("data/W_pvals_and_thetas_allcounties.Rdata")
load("data/processed_dat.RData")

dat <- cbind(populations, theta1, theta2, pvals)
dat$county <- gsub(" County", "", dat$County.Name)
dat$county <- gsub(" Parish", "", dat$county)
dat$county <- tolower(dat$county)
dat$state <- dat$State

dat$dtheta <- dat$theta1 - dat$theta2

## a clamped version of dtheta
clamp <- function(x, c) {
    x[x > c] <- c
    x[x < -c] <- -c
    return(x)
}
dat$dtheta_clamped <- clamp(dat$dtheta, 20)

choropleth2 <- merge(county_df, dat, all.x = TRUE,
    by = c("state", "county")
)

choropleth2 <- choropleth2[order(choropleth2$order), ]

p2 <- ggplot(choropleth2, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = dtheta_clamped), colour = alpha("white", 1 / 2), linewidth = 0.2) +
    geom_polygon(data = state_df, colour = "white", fill = NA) +
    coord_fixed() +
    theme_minimal() +
    ggtitle("Difference in k over Thanksgiving 2020") +
    theme(
        axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()
    ) +
    scale_fill_viridis(option = "magma")


# Save map 
# library(gridExtra)
# 
# filename <- "./figures/map_difftheta_US.pdf"
# 
# pdf(filename, width = 6, height = 6)
# 
# p2
# 
# dev.off()