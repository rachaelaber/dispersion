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

load("data/lthetadiffs_lg_pops.Rdata")

z <- t(lthetadiffs)
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
