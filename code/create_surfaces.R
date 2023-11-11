# Plot likelihood ratios as a surface over counties and time

rm(list = ls())

graphics.off()

library(viridis)

# Load data

load("data/lrt_lg_pops.Rdata")

load("data/processed_long_dat.Rdata")   

# Data setup

dates <- dates[30:(length(dates) - 30 + 1)]
z <- t(lrt_stats)
x <- seq_len(nrow(z))
y <- seq_len(ncol(z))

filename <- "figures/lrt_surface_figure.pdf"

pdf(filename, width = 8, height = 8)

# Plot

par(pin = c(7, 5))

image(x, y, z,
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "Locations",
      col = rev(mako(32)))

par(las = 2)
axis(1, x, dates, tick = FALSE)

dev.off()

# Dates of bands:
# Just after 2021-03-01; just after 2021-05-15; 2021-04-05
# 2021-01-10 seen across many of the counties
# Color by direction of theta change by making same matrix w/
# theta change

load("data/thetadiff_lg_pops.Rdata")

z <- t(thetadiffs)
x <- seq_len(nrow(z))
y <- seq_len(ncol(z))

filename <- "figures/thetadiff_surface.pdf"

pdf(filename, width = 8, height = 8)

# Plot
par(pin = c(7, 5))
image(x, y, z,
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "Locations",
      col = rev(mako(32)))

par(las = 2)
axis(1, x, dates, tick = FALSE)

dev.off()
