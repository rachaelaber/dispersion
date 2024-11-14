rm(list = ls())

# Load data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/theta_lg_pops.Rdata")

filename <- "figures/fig2.pdf"
pdf(filename, width = 6, height = 6)

# Presumed reporting rate
reporting_rate1 <- 1
reporting_rate2 <- 0.25

# Trim to same length as outputs
series <- cases[1, 8:(length(cases[1, ]) - 8 + 1)]
dates <- dates[8:(length(cases[1, ]) - 8 + 1)]

# Remove first observations from all
# series <- series[35:length(series)]
# dates <- dates[35: length(dates)]
# lrt_stats <- lrt_stats[1, 35:dim(lrt_stats)[2]]
# thetas <- thetas[1, 35:dim(thetas)[2]]

# Plot

par(mfrow = c(2, 1))
par(mar = c(4, 5, 2, 2))
plot(dates, series / 1000,
  type = "h", lwd = 2, col = "#34304a", cex.main = 1.3, cex.lab = 1, cex.axis = 1,
  ylim = c(0, 20), ylab = "Cases (thousands)", xlab = "", yaxt = "n"
)
axis(2, seq(0, 20, 10))
mtext("a", side = 3, line = 1, adj = 0, cex = 1.3)


z <- lrt_stats[1, ]
z[is.na(z)] <- 0
z <- z / max(z)
is_sig <- lrt_stats[1, ] > qchisq(0.9996753, df = 1) # w/o correcting for multiple testing
plot(dates, log10(thetas[1, ]),
  type = "n",
  xlab = "", ylab = expression(log[10](theta)),
  # main = expression(bold(log10(theta))),
  cex.main = 1.3, cex.lab = 1, cex.axis = 1, ylim = c(-0.5, 3)
)
for (i in 1:(length(dates) - 1)) {
  segment_color <- rgb(z[i], 0.85*(1 - z[i]), 1 - z[i])
  x <- dates[i:(i + 1)]
  y <- log10(thetas[1, i:(i + 1)])
  yl <- y - 0.1 - 0.2 * z[i:(i + 1)]
  yu <- y + 0.1 + 0.2 * z[i:(i + 1)]
  xx <- c(x, rev(x))
  yy <- c(yl, rev(yu))
  polygon(xx, yy, col = segment_color, border = NA)
}

lines(dates, log10(c(NA, series[-length(series)])) - log10(reporting_rate1), lwd = 1, col = 1, lty = 2)
lines(dates, log10(c(NA, series[-length(series)])) - log10(reporting_rate2), lwd = 1, col = 1, lty = 3)



# legend("bottomleft",
#  legend = c(expression(chi^2 >= chi[crit]^2), expression(chi^2 < chi[crit]^2)),
#  lty = 1, col = c("red", "blue"), cex = 0.6, bty = "n"
# )
mtext("b", side = 3, line = 1, adj = 0, cex = 1.3)

dev.off()
