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


# Define common x-axis ticks and labels
start_date_label <- as.Date("2020-07-01")
end_date_label <- as.Date("2022-07-01")
at <- seq.Date(
  from = start_date_label,
  to = end_date_label,
  by = "year"
)
labels <- format(at, format = "%b %y") 



# Plot
filename <- "figures/fig3.pdf"
pdf(filename)
par(mfrow = c(3, 2))

# Cases time series
par(mar = c(4, 4, 3, 6))

plot(dates, colSums(cases) / sum(pops) * 1000,
  type = "l", lwd = 2, col = "black", xlab = "",
  xaxt = "n",
  ylab = "",
  ylim = c(0, 20),
  main = "Cases per 1000 pop",
  cex.main = 1.7, cex.lab = 1.4
)

par(xpd = TRUE)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.5)
par(xpd = FALSE)

axis(1, at = at, labels = labels, cex.axis = 1.3)


# Mean theta time series
par(mar = c(4, 4, 3, 6))

plot(dates, log10(colMeans(thetas, na.rm = TRUE)),
  type = "l", lwd = 2, col = "black", xlab = "",
  ylab = "",
  main = expression(bold("Mean " * log[10](theta))),
  cex.main = 1.7,
  cex.lab = 1.4,
  yaxt = "n",
  xaxt = "n",
  ylim = c(0.8, 3)
)
axis(2, c(1, 2, 3))

par(xpd = TRUE)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.5)

axis(1, at = at, labels = labels, cex.axis = 1.3)

# Cases surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- cases

breaks <- c(0, 10^seq(0, 6, len = 20))
breaks[9] <- 10^2
breaks[15] <- 10^4

colors <- turbo(length(breaks) - 3)[-1]
colors <- c(colors[1], colors[1], colors[1], colors)

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "",
  main = "Cases",
  cex.main = 1.7,
  cex.lab = 1.4
)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("c", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^2, 10^4, 10^6),
  fill = colors[c(2, 9, 15, 20)],
  cex = 0.8
)



# Theta surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(thetas)

breaks <- c(-2, 0, 1, 1.5, 2, 3, 4, 5, 6)

colors <- rev(turbo(length(breaks) - 3))
colors <- c(colors, tail(colors, 1), tail(colors, 1))

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "",
  main = expression(paste(theta)),
  cex.main = 1.7,
  cex.lab = 1.4
)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("d", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^1, 10^2, 10^3, 10^4, 10^5),
  fill = colors[c(2, 3, 5, 6, 7, 8)],
  cex = 0.8
)


# theta0 (null expectation for theta)
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(cases / reporting_rate)

breaks <- c(-2, 0, 1, 1.5, 2, 3, 4, 5, 6)

colors <- rev(turbo(length(breaks) - 3))
colors <- c(colors, tail(colors, 1), tail(colors, 1))

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "",
  main = expression(paste(theta[null])),
  cex.main = 1.7,
  cex.lab = 1.4
)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("e", side = 3, line = 1, adj = 0, cex = 1.5)

legend("topright",
  inset = c(-0.34, 0.1),
  legend = expression(10^0, 10^1, 10^2, 10^3, 10^4, 10^5),
  fill = colors[c(2, 3, 5, 6, 7, 8)],
  cex = 0.8
)





# p-values surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- lrt_ps
lower_range <- c(0, 10^seq(-20, -2, len = 3))
upper_range <- c(1 / 20, 0.2, 0.5, 1)
breakpoints <- c(lower_range, upper_range)
nbreakpoint <- length(breakpoints)

colors <- c(mako(nbreakpoint - 3), "#ffffffd7", "white")

image(dates,
  seq_len(nrow(x)),
  t(x),
  col = colors,
  breaks = breakpoints,
  ylab = "",
  yaxt = "n",
  xaxt = "n",
  xlab = "",
  main = expression(P(theta[1] == theta[2])),
  cex.main = 1.7,
  cex.lab = 1.4
)

axis.Date(1, at = at, labels = labels, cex.axis = 1.3)

mtext("f", side = 3, line = 1, adj = 0, cex = 1.5)

legend(
  "topright",
  inset = c(-0.27, 0.1),
  legend = c(
    expression(10^-11),
    expression(10^-2),
    "0.05", "0.2", "1"
  ),
  fill = colors[-1],
  cex = 0.8
)

dev.off()
