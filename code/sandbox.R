rm(list = ls())
graphics.off()

library(viridis)


# Prep data
load("data/lrt_lg_pops_weekly.Rdata")
load("data/new_cases_lg_weekly.Rdata")
load("data/theta_lg_pops_weekly.Rdata")
load("data/lrtps_lg_pops_weekly.Rdata")
load("data/processed_long_dat.Rdata")


# Trim elements or columns that will have NAs for theta and lrt
keep <- 30 : (length(dates) - 30 + 1)
dates <- dates[keep]
new_cases_lg_weekly <- new_cases_lg_weekly[, keep]



# Functions
clamp <- function(x, a, b) {

    y <- x
    y[y < a] <- a
    y[y > b] <- b

    return(y)

}


# Plot

filename <- "figures/roughdraft_surfaces.pdf"

pdf(filename, width = 8, height = 8)

i <- 100

par(mfrow = c(2, 2))

x <- colMeans(new_cases_lg_weekly)
plot(dates, x / sum(x, na.rm = T), type = 'l', lwd = 3)

x <- new_cases_lg_weekly[1, ]
lines(dates, x / sum(x, na.rm = T), col = 2)

x <- new_cases_lg_weekly[100, ]
lines(dates, x / sum(x, na.rm = T), col = 2)



x <- clamp(thetas, 0, 10000)
x <- colMeans(x, na.rm = TRUE)
plot(dates, x, type = 'l', lwd = 3)

# reasoning for clamping:
# as theta increases much beyond 30, it is not practically different
# than poisson. (see figures/ecdfs.pdf)

x <- clamp(thetas, 0, 30)

image(dates,
      1:nrow(x),
      t(x),
      col = rev(viridis(32)),
      xaxt = "n")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)

#

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
      xaxt = "n")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)


dev.off()