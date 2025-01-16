rm(list = ls())
graphics.off()

library(viridis)

# Prep data
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/nyt_weekly.Rdata")
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/lrtps_lg_pops.Rdata")
load("data/processed/ftr_poiss_lg_pops.Rdata")
load("data/processed/ctzs_lg_pops.Rdata")


# Parameters
reporting_rate <- 0.10
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
  ylim = c(1, 5)
)
par(xpd = TRUE)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.5)


# c. cases surface
par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- log10(cases / 0.1)

breaks <- log10(10^(seq(0, 5, len = 10)))

colors <- c(viridis(length(breaks) - 1))

image(dates, 1:144, t(x),
  breaks = breaks,
  col = colors,
  yaxt = "n",
  ylab = "",
  xaxt = "n",
  xlab = "Date",
  main = expression(bold(log10(theta[0]))),
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
  legend = rep("", length(colors)),
  fill = colors,
  cex = 0.8
)

# 4. log10(thetas) image

par(mar = c(4, 4, 3, 6), xpd = TRUE)

x <- thetas

breaks <- log10(c(1, 3, 10, 100, 1000, 10^20))

colors <- c(rev(viridis(length(breaks) - 1)))

image(dates, 1:144, t(log10(x)),
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
  legend = c("< log10(3)", "< 1", "< 2", "< 3", "< 20"),
  fill = colors,
  cex = 0.8
)

# 5. log10(EXPECTED thetas) barplot

par(mar = c(4, 4, 3, 6), xpd = TRUE)

thetas <- cases / reporting_rate
my <- format(dates, "%b-%y")
umy <- unique(my)
nmy <- length(umy)

breaks <- log10(c(1, 3, 10, 100, 1000, 10^20))

x <- matrix(NA, nrow = length(breaks) - 1, ncol = nmy)

for (i in 1:nmy) {
  these_thetas <- thetas[, my == umy[i]]
  x[, i] <- as.numeric(table(cut(log10(these_thetas), breaks = breaks)))
}

colors <- c(rev(viridis(length(breaks) - 1)))

# Store barplot result to get x positions for the bars
bar_positions <- barplot(x,
  col = colors,
  names.arg = substr(umy, 1, 1),
  xlab = "Date",
  xaxt = "n", # Disable automatic x-axis to add manually
  ylab = "Number of estimates",
  cex.main = 1.7,
  cex.lab = 1.4,
  main = expression(bold(log10(theta[0])))
)

mtext("e", side = 3, line = 1, adj = 0, cex = 1.5)

at <- seq(1, nmy, by = 1) # One for each month
labels <- substr(umy, 1, 1) # First character of each month
axis(1, at = bar_positions, labels = labels, cex.axis = 1.2)

legend("topright",
  inset = c(-0.31, 0.1),
  legend = c("< log10(3)", "< 1", "< 2", "< 3", "< 20"),
  fill = colors,
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

legend("topright",
  inset = c(-0.27, 0.1),
  legend = c(
    expression("" < 10^{
      -20
    }), expression("" < 10^{
      -11
    }), expression("" < 10^{
      -2
    }),
    "< 0.05", "< 0.10", "< 0.55", "< 1"
  ),
  fill = cols,
  cex = 0.8
)

dev.off()
