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

par(mfrow = c(2, 2))

x <- colMeans(new_cases_lg_weekly)
plot(dates, x / sum(x, na.rm = T), type = 'l', lwd = 3)

x <- new_cases_lg_weekly[1, ]
lines(dates, x / sum(x, na.rm = T), col = 2)

x <- new_cases_lg_weekly[100, ]
lines(dates, x / sum(x, na.rm = T), col = 2)



x <- clamp(thetas, 0, 30)
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





# Questions: 

# 1. Estimate of theta constrained to be
# between 1.000045e-20 and 19.999525e+19. 
max(thetas, na.rm = TRUE)
min(thetas, na.rm = TRUE)

# 2. The density of log10(thetas) appears 'clumped'
# with a cluster near 0, one near 10 and one near 15.
# And then there is a lot of density focused right near -20.
# Why would this be? For this and question 1 above I'm wondering if 
# there is an optimizer that is hitting boundary counditions
# during the estimation of theta. Just a wild guess.

# Since the lrt function uses the irls.nb.1 function to fit the
# model, we might be running into the fact that an initial value of 
# phi = 1 is used, and also the optimize call has the following args 
# in all the likelihood optimizations (see lrt.R):
# obj0 = optimize(l0, c(log(1e-20), log(1e20)), maximum=TRUE)
quantile(log10(thetas), na.rm = TRUE)
hist(log10(thetas), breaks = 200)


# 3. This method of making an image of thetas, 
# using roughly the quartiles of log10(thetas),
# seems to work. Each color has a simple meaning.
# And does not require arbitrary clamping.
# Should we use it instead?
image(t(log10(thetas)), 
        breaks = c(-20, 1, 2, 10, 20),
        col = rev(viridis(4)))

# I think so, although somewhere we should highlight the info above
# about the range in which theta is being optimized


# 4. The quantiles of log10(thetas) over time look strange. 
# They seem to sometimes 'stick' to particular values then change suddenly.
# This may be related to #1 and #2
q1 <- apply(log10(thetas), MARGIN = 2, FUN = quantile, 0.2, na.rm = TRUE)
q2 <- apply(log10(thetas), MARGIN = 2, FUN = quantile, 0.4, na.rm = TRUE)
q3 <- apply(log10(thetas), MARGIN = 2, FUN = quantile, 0.6, na.rm = TRUE)
q4 <- apply(log10(thetas), MARGIN = 2, FUN = quantile, 0.8, na.rm = TRUE)

plot(dates, q1, type = "l", ylim = c(-20, 20))
lines(dates, q2, col = 2)
lines(dates, q3, col = 3)
lines(dates, q4, col = 4)

# I wonder if this is basically the "true" value of theta being similar
# at points close in time, but the algorithm estimates something near a
# boundary or near 1


# 5. Searching for simpler and more transparent ways to
# show the signal we're on to. Maybe something like this?
# Could we use this instead of "average theta" in the top
# right corner of the plot above?
# Two versions are here, with and without NAs.
# They "agree" but each have strengths

#  Version 1 where NAs propegate.
#  The shape of the time series is not affected by 
#  the number missing at each time point.
is_overdispersed <- log10(thetas) < 2
noverdispersed <- colSums(is_overdispersed, na.rm = FALSE)
plot(dates, noverdispersed, xaxt = "n")

#  custom date labels
at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)
par(cex.axis = 0.7)
axis.Date(1, at = at, labels = labels)


#  Version 2 where NAs are removed.
#  The shape of the time series may be affected
#  by the number missing at each time point.
is_overdispersed <- log10(thetas) < 2
noverdispersed <- colSums(is_overdispersed, na.rm = TRUE)
plot(dates, noverdispersed, xaxt = "n")
par(cex.axis = 0.7)
axis.Date(1, at = at, labels = labels)


#6. For the upper right plot of overall trends in theta
#   over time, What about a sort of stacked bar plot
#   where for every month we say what proportion
#   of the counties where between each quartile?

my <- format(dates, "%b-%y")
umy <- unique(my)
nmy <- length(umy)

breaks <- c(-20, seq(1, 10, len = 8), 20)
x <- matrix(NA, nrow = length(breaks) - 1, ncol = nmy)

for (i in 1:nmy) {
      these_thetas <- thetas[, my == umy[i]]
      these_thetas[log10(these_thetas) < -3] <- NA    #TEMPORARY experimenting with suppressing very small thetas which I am suspecting might be a kind of failure in the estimatation process, like hitting boundary conditions in the optimizer?
      x[, i] <- as.numeric(table(cut(log10(these_thetas), breaks = breaks)))
}

barplot(x,
      col = rev(viridis(length(breaks) - 1)),
      names.arg = substr(umy, 1, 1))


#7. How do the spikes in the number of overdispersed 
#time series compare with incidence? The were more overdispersed
#counties near the spikes in incidence, which is potentially
#interesting.

#TODO: the two plots are not properly lined up in terms
# of the months.

incidence <- matrix(NA,
                    nrow = nrow(new_cases_lg_weekly),
                    ncol = ncol(new_cases_lg_weekly))
for (i in seq_len(nrow(new_cases_lg_weekly))) {
      incidence[i, ] <- new_cases_lg_weekly[i,] / populations_lg$population[i]
}

par(mfrow = c(2, 1))
barplot(x,
      col = rev(viridis(length(breaks) - 1)),
      names.arg = substr(umy, 1, 1))

m <- substr(format(dates, "%b"), 1, 1)
image(dates, 1:150, t(log10(incidence)),
      xaxt = "n",
      xlab = "",
      ylab = "",
      yaxt = "n")

at <- seq.Date(from = min(dates), to = max(dates), by = 'month')
labels <- format(at, format = "%b")
labels <- substr(labels, 1, 1)

axis.Date(1, at = at, labels = labels)