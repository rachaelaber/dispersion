rm(list = ls())
graphics.off()


library(viridis)


# Load data
load("data/processed/new_cases_lg_weekly.Rdata")
load("data/processed/theta_lg_pops_weekly.Rdata")
load("data/processed/processed_long_dat.Rdata") # for dates
rm(new_cases, populations)
theta <- thetas2
cases <- new_cases_lg_weekly
pops <- populations_lg$population
rm(thetas1, thetas2, thetas)
rm(populations_lg)
rm(new_cases_lg_weekly)


# Trim cases and dates to match theta
keep <- 30:(length(dates) - 30 + 1)
dates <- dates[keep]
cases <- cases[, keep]
rm(keep)



# Construct incidence
n <- nrow(cases)
m <- ncol(cases)
incidence <- matrix(NA, n, m)
for (i in 1:n) {
  incidence[i, ] <- cases[i, ] / pops[i]
}
rm(i, n, m)





# Plot
# theta_clamped <- theta
# theta_clamped[log10(theta) > 6] <- 10^6
# theta_clamped[log10(theta) < -6] <- 10^-6

filename <- "figures/fig3.pdf"

pdf(filename, width = 8, height = 8)

frame1 <- 101:500
frame2 <- 501:650
frame3 <- 651:1000
frames <- list(frame1, frame2, frame3)

pal <- rev(viridis(900))

par(pin = c(1.5, 1.5))
par(mfrow = c(1, 3))

for (i in 1:3) {
  frame <- frames[[i]]
  x <- log10(colMeans(incidence)[frame])
  y <- log10(colMeans(theta, na.rm = TRUE)[frame])
  plot(x, y,
    pch = 21,
    col = pal[frame - 100],
    cex = 2,
    cex.lab = 1.5,
    xlab = "log_10 Mean incidence",
    ylab = "log_10 Mean theta",
    xaxt = "n",
    xlim = c(-5, -2),
   # ylim = c(4.75, 6),
    yaxt = "n",
    bty = "n"
  )
  axis(1,
    seq(-5, -2, 1),
    cex.axis = 1
  )
  axis(2,
    seq(5, 6, 0.5),
    cex.axis = 1
  )
  mtext(letters[1:3][i], side = 3, line = 1, adj = 0)
  
}
rm(i, x, y, frame, frames)

dev.off()
