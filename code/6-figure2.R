rm(list = ls())
graphics.off()

library(viridis)

# Prep data
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/nyt_weekly.Rdata")
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/lrtps_lg_pops.Rdata")


# Trim elements or columns that will have NAs for theta and lrt
keep <- 8:(length(dates)-8+1)
dates <- dates[keep]
cases <- cases[, keep]
incidence <- incidence[, keep]


# Plot

filename <- "figures/fig2.pdf"

pdf(filename)

par(mfrow = c(2, 2))


# 1

thetas <- thetas2
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
                         xaxt = "n",  # Disable automatic x-axis to add manually
                         ylab = expression(log10(theta)),
                         main = expression(log10(theta)),
                         cex.axis = 1,
                         cex.main = 1.7,
                         cex.lab = 1.4)

mtext("a", side = 3, line = 1, adj = 0, cex = 1.7)

legend("topright", 
       legend = c("1 (most dispersed)", "2", "3", "4", "5"), 
       fill = colors,
       bg = "white",
       cex = .6)

# Manually adjust x-axis tick marks and labels
at <- seq(1, nmy, by = 1)  # One for each month
labels <- substr(umy, 1, 1)  # First character of each month
axis(1, at = bar_positions, labels = labels, cex.axis = 1.2)

# 2

x <- thetas2

image(dates, 1:144, t(log10(x)), 
        breaks = breaks,
        col = colors, 
        yaxt = "n",
        ylab = "",
        xaxt = "n",
        xlab = "Date",
        main = expression(log10(theta)),
        cex.main = 1.7,
        cex.lab = 1.4)

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti, cex.axis = 1.3)

mtext("b", side = 3, line = 1, adj = 0, cex = 1.7)


# 3
breaks <- seq(min(cases), max(cases), length.out = 8)
breaks <- breaks[-c(1, 8)]
x <- matrix(NA, nrow = length(breaks) - 1, ncol = nmy)

for (i in 1:nmy) {
  these <- cases[, my == umy[i]]
  x[, i] <- as.numeric(table(cut(these, breaks = breaks)))
}

colors <- c(rev(viridis(length(breaks) - 1)))

bar_positions <- barplot(x,
                         col = colors,
                         names.arg = substr(umy, 1, 1), 
                         xlab = "Date",
                         xaxt = "n",  # Disable automatic x-axis to add manually
                         ylab = "Cases",
                         main = "Cases",
                         cex.axis = 1,
                         cex.main = 1.7,
                         cex.lab = 1.4)

mtext("c", side = 3, line = 1, adj = 0, cex = 1.7)

mean_cases <- colMeans(x)
spline_cases <- spline(1:nmy, mean_cases)
lines(bar_positions[spline_cases$x], spline_cases$y, col = "red", lwd = 2)

legend("topleft", 
       legend = c("1 (lowest)", "2", "3", "4", "5", "Mean cases"), 
       fill = c(colors, NA),
       border = NA,
       lty = c(NA, NA, NA, NA, NA, 1),
       col = c(rep("black", 5), "red"),
       bg = "white",
       cex = .6)

# Manually adjust x-axis tick marks and labels
at <- seq(1, nmy, by = 1)  # One for each month
labels <- substr(umy, 1, 1)  # First character of each month
axis(1, at = bar_positions, labels = labels, cex.axis = 1.2)


# 4

x <- lrt_ps
lower_range <- c(0, 10^seq(-50, -2, len = 12))
upper_range <- c(1/20, seq(0.1, 1, len = 8))
breakpoints <- c(lower_range, upper_range)
nbreakpoint <- length(breakpoints)


cols <- rev(viridis(nbreakpoint - 1))
x[is.na(x)] <- 1
image(dates,
      1:nrow(x),
      t(x),
      col = cols,
      breaks = breakpoints,
      ylab = "",
      yaxt = "n",
      xaxt = "n",
      xlab = "Date",
      main = "p-value",
      cex.main = 1.7,
      cex.lab = 1.4)

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, cex.axis = 1.3)

mtext("d", side = 3, line = 1, adj = 0, cex = 1.7)

dev.off()
