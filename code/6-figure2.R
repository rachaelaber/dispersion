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

barplot(x,
      col = colors,
      names.arg = substr(umy, 1, 1), 
      xlab = "Date",
      xaxt = "n",
      ylab = expression(theta),
      main = "Dispersion")

mtext("a", side = 3, line = 1, adj = 0)

legend("topright", 
       legend = c("1 (most dispersed)", "2", "3", "4", "5"), 
       fill = colors,
       bg = "white",
       cex = 0.5)

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti)

# 2

x <- thetas2

image(dates, 1:144, t(log10(x)), 
        breaks = breaks,
        col = colors, 
        yaxt = "n",
        ylab = "",
        xaxt = "n",
        xlab = "Date",
        main = "Dispersion")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti)

mtext("b", side = 3, line = 1, adj = 0)

# 3
incidence <- matrix(NA,
                    nrow = nrow(cases),
                    ncol = ncol(cases))
for (i in seq_len(nrow(cases))) {
  incidence[i, ] <- cases[i,] / county$population[i]
}


m <- substr(format(dates, "%b"), 1, 1)
image(dates, 1:144, t(log10(incidence)),
      xaxt = "n",
      ylab = "",
      yaxt = "n",
      xlab = "Date",
      main = "Incidence")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)

mtext("c", side = 3, line = 1, adj = 0)

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
      main = "p-value")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)

mtext("d", side = 3, line = 1, adj = 0)

dev.off()
