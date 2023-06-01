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

choropleth <- merge(county_df, unemp, by = c("state", "county"))
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




# Adapt the above to plot thanksgiving results
load("data/W_pvals_and_thetas_allcounties.Rdata")
load("data/processed_dat.RData")

dat <- cbind(populations, theta1, theta2, pvals)
dat$county <- tolower(gsub(" County", "", dat$County.Name))
dat$state <- dat$State

dat$dtheta <- dat$theta1 - dat$theta2

# A clamped version of dtheta
clamp_tol <- 20
dat$dtheta_trimmed <- dat$dtheta
dat$dtheta_trimmed[dat$dtheta_trimmed > clamp_tol] <- clamp_tol
dat$dtheta_trimmed[dat$dtheta_trimmed < -clamp_tol] <- -clamp_tol

choropleth2 <- merge(county_df, dat,
    by = c("state", "county")
)

choropleth2 <- choropleth2[order(choropleth2$order), ]

p2 <- ggplot(choropleth2, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = dtheta_trimmed), colour = alpha("white", 1 / 2), linewidth = 0.2) +
    geom_polygon(data = state_df, colour = "white", fill = NA) +
    coord_fixed() +
    theme_minimal() +
    ggtitle("theta2-theta1 (clamped)") +
    theme(
        axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()
    ) +
    scale_fill_viridis(option = "magma")


# Plot the 2 maps together
library(gridExtra)
grid.arrange(p1, p2, nrow = 2)