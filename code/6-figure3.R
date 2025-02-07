rm(list = ls())
graphics.off()

library(viridis)

# Prep data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/theta_lg_pops.Rdata")


# Parameters
reporting_rate <- 0.5
ww <- 8


# Trim elements or columns that will have NAs for theta and lrt
keep <- ww:(length(dates) - ww)
dates <- dates[keep]
cases <- cases[, keep]
incidence <- incidence[, keep]


# Plot
filename <- "figures/fig3.pdf"
pdf(filename)
par(mfrow = c(3, 2))

# a. Mean cases
par(mar = c(4, 4, 3, 6))

plot(dates, colMeans(cases),
  type = "l", lwd = 2, col = "black", xlab = "Date",
  ylab = "", main = "Mean cases", cex.main = 1.7, cex.lab = 1.4
)

par(xpd = TRUE)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.5)
par(xpd = FALSE)


# b. Mean log10(theta)
par(mar = c(4, 4, 3, 6))

plot(dates, colMeans(log10(thetas), na.rm = TRUE),
  type = "l", lwd = 2, col = "black", xlab = "Date",
  ylab = "",
  main = expression(bold("Mean " * log[10](theta))),
  cex.main = 1.7,
  cex.lab = 1.4,
  yaxt = "n",
  ylim = c(0.8, 2.2)
)
axis(2, c(1, 1.5, 2))

par(xpd = TRUE)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.5)




# c. cases surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(cases)

breaks <- c(-1, 0, 1, 1.5, 2, 3, 4)

colors <- viridis(length(breaks) - 1)

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "Date",
  main = expression(bold(log10(cases))),
  cex.main = 1.7,
  cex.lab = 1.4
)

at <- seq.Date(from = min(dates), to = max(dates), by = "month")
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("c", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^1, 10^2, 10^4),
  fill = colors[c(2, 3, 5, 6)],
  cex = 0.8
)

# d. log10(thetas) image

par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(thetas)

breaks <- c(-1, 0, 1, 1.5, 2, 3, 4)

colors <- viridis(length(breaks) - 1)

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "Date",
  main = expression(bold(log10(theta))),
  cex.main = 1.7,
  cex.lab = 1.4
)

at <- seq.Date(from = min(dates), to = max(dates), by = "month")
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("d", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^1, 10^2, 10^4),
  fill = colors[c(2, 3, 5, 6)],
  cex = 0.8
)


#e. theta0 surface

# d. log10(thetas) image

par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(cases / reporting_rate)

breaks <- c(-1, 0, 1, 1.5, 2, 3, 4)

colors <- viridis(length(breaks) - 1)

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "Date",
  main = expression(bold(log10(theta0))),
  cex.main = 1.7,
  cex.lab = 1.4
)

at <- seq.Date(from = min(dates), to = max(dates), by = "month")
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("e", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^1, 10^2, 10^4),
  fill = colors[c(2, 3, 5, 6)],
  cex = 0.8
)

# 6. p-values surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- lrt_ps
lower_range <- c(0, 10^seq(-20, -2, len = 3))
upper_range <- c(1 / 20, seq(0.1, 1, len = 3))
breakpoints <- c(lower_range, upper_range)
nbreakpoint <- length(breakpoints)

cols <- rev(viridis(nbreakpoint - 1))

image(dates,
  seq_len(nrow(x)),
  t(x),
  col = cols,
  breaks = breakpoints,
  ylab = "",
  yaxt = "n",
  xaxt = "n",
  xlab = "Date",
  main = "p-value",
  cex.main = 1.7,
  cex.lab = 1.4
)

at <- seq.Date(from = min(dates), to = max(dates), by = "month")
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, cex.axis = 1.3)

mtext("f", side = 3, line = 1, adj = 0, cex = 1.5)

legend(
  "topright",
  inset = c(-0.27, 0.1),
  legend = c(
    expression(10^-11), 
    expression(10^-2),
    "0.05", "0.10", "0.55", "1"
  ),
  fill = cols[-1],
  cex = 0.8
)

dev.off()
