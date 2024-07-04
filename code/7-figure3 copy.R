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
#rm(thetas1, thetas2, thetas)
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

#filename <- "figures/fig3.pdf"

#pdf(filename, width = 8, height = 8)

frame1 <- 101:500
frame2 <- 501:800
frame3 <- 801:1124
frames <- list(frame1, frame2, frame3)

pal <- rev(viridis(1024))


par(mfcol = c(2, 3))

for (i in 1:3) {
  frame <- frames[[i]]
  
  thet <- theta
  thet[log10(theta) > 5] <- NA
  thet[log10(theta) < -5] <- NA

  x <- colMeans(log10(incidence + 10^-7))[frame]
  y <- colMeans(log10(thet), na.rm = TRUE)[frame]
  
  par(mar = c(7, 7, 20, 5))
  plot(x, y,
    pch = 19,
    col = pal[frame - 100],
    cex = 1,
    cex.lab = 1.5,
    xlab = "Mean log_10 incidence",
    ylab = "Mean log_10 theta",
    xaxt = "n",
    xlim = c(-6, -3),
    ylim = c(0.2, 2),
    yaxt = "n",
    bty = "n"
  )
  axis(1,
    seq(-7, -2, 1),
    cex.axis = 1
  )
  axis(2,
    seq(1, 5, 1),
    cex.axis = 1
  )
  mtext(letters[1:3][i], side = 3, line = 1, adj = 0)
  
  # add lines for every place?

  #
  par(mar = c(20, 7, 5, 5))
  plot(colMeans(incidence + 10^-7), col = 'grey')
  points(frame, colMeans(incidence + 10^-7)[frame], col = pal[frame - 100], pch = 19)


}
rm(i, x, y, frame, frames)




#dev.off()
