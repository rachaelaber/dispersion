rm(list = ls())
graphics.off()

library(viridis)

# Prep data
load("data/processed/lrt_lg_pops_weekly.Rdata")
load("data/processed/new_cases_lg_weekly.Rdata")
load("data/processed/theta_lg_pops_weekly.Rdata")
load("data/processed/lrtps_lg_pops_weekly.Rdata")
load("data/processed/processed_long_dat.Rdata")


# Trim elements or columns that will have NAs for theta and lrt
keep <- 30 : (length(dates) - 30 + 1)
dates <- dates[keep]
new_cases_lg_weekly <- new_cases_lg_weekly[, keep]


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
      these_thetas[log10(these_thetas) < -3] <- NA    #TEMPORARY experimenting with suppressing very small thetas which I am suspecting might be a kind of failure in the estimatation process, like hitting boundary conditions in the optimizer?
      x[, i] <- as.numeric(table(cut(log10(these_thetas), breaks = breaks)))
}

colors <- c(rev(viridis(length(breaks) - 1)))
barplot(x,
      col = colors,
      names.arg = substr(umy, 1, 1), 
      xlab = "Date", 
      ylab = expression(theta))

legend("topright", col = colors, cex = .4, 
       legend = c("Most dispersed", "Very Dispersed", "Dispersed", "Somewhat Dispersed", "Approaching Poisson"), 
       lty = 1)

# Used entry [11,6] in legend example
# 2

x <- thetas2

image(dates, 1:150, t(log10(x)), 
        breaks = breaks,
        col = colors, 
        yaxt = "n",
        ylab = "",
        xaxt = "n",
        xlab = "Date")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels, ti)

# 3
incidence <- matrix(NA,
                    nrow = nrow(new_cases_lg_weekly),
                    ncol = ncol(new_cases_lg_weekly))
for (i in seq_len(nrow(new_cases_lg_weekly))) {
  incidence[i, ] <- new_cases_lg_weekly[i,] / populations_lg$population[i]
}


m <- substr(format(dates, "%b"), 1, 1)
image(dates, 1:150, t(log10(incidence)),
      xaxt = "n",
      ylab = "",
      yaxt = "n",
      xlab = "Date")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)

# 4

x <- lrt_ps
lower_range <- c(0, 10^seq(-50, -2, len = 12))
upper_range <- c(1/20, seq(0.1, 1, len = 8))
breakpoints <- c(lower_range, upper_range)
nbreakpoint <- length(breakpoints)

cols <- rev(cividis(nbreakpoint - 1))
x[is.na(x)] <- 1
image(dates,
      1:nrow(x),
      t(x),
      col = cols,
      breaks = breakpoints,
      ylab = "",
      yaxt = "n",
      xaxt = "n",
      xlab = "Date")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)


dev.off()
