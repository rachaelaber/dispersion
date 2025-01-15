rm(list = ls())
graphics.off()


# Parameters
ww <- 8 # window half-width


# Load data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/lrt_lg_pops.Rdata")
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/ftr_poiss_lg_pops.Rdata")
load("data/processed/ctzs_lg_pops.Rdata")

filename <- "figures/fig2.pdf"
pdf(filename, width = 6, height = 6)


# Trim cases to same frame as theta estimates
# and pick one county for plotting
ndate <- ncol(cases)
date_idx <- ww:(ndate - ww)
target_county <- 1
pop <- pops[target_county]
cases <- cases[target_county, date_idx]
dates <- dates[date_idx]
ctzs <- ctzs[target_county, ]
ftr_poiss <- ftr_poiss[target_county, ]
thetas <- thetas[target_county, ]
thetas1 <- thetas1[target_county, ]
thetas2 <- thetas2[target_county, ]
thetas[ctzs] <- NA
thetas1[ctzs] <- NA
thetas2[ctzs] <- NA
thetas[ftr_poiss] <- NA
thetas1[ftr_poiss] <- NA
thetas2[ftr_poiss] <- NA



# Plot
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 2))


# a
plot(dates, cases / pop * 1000,
  type = "h",
  xlab = "",
  ylab = "Weekly cases per 1000 pop.",
  cex.lab = 1,
  ylim = c(0, 30),
  cex.axis = 1,
  yaxt = "n"
)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.3)
axis(2, seq(0, 30, 10))

# b
plot(dates, log10(thetas),
  type = "l",
  xlab = "",
  ylab = expression(paste(log[10], "(", theta, ")")),
  cex.main = 1.3,
  ylim = c(-0.2, 3),
  cex.lab = 1,
  cex.axis = 1
)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.3)


# c
theta0a <- c(NA, cases[-length(cases)] / 0.1)
theta0b <- c(NA, cases[-length(cases)] / 0.9)
plot(dates, log10(theta0a),
  type = "l", ylim = c(0, 6), xlab = "",
  ylab = expression(paste(log[10], "(", theta, ")"))
)
lines(dates, log10(theta0b))
lines(dates, log10(thetas), col = "grey")
mtext("c", side = 3, line = 1, adj = 0, cex = 1.3)


# d
is_sig <- lrt_stats[1, ] > qchisq(0.9996732, df = 1)

pch <- ifelse(is_sig, 19, 1)
col <- ifelse(is_sig, 2, 1)
cex <- ifelse(is_sig, 1.25, 0.75)
plot(dates, lrt_stats[target_county, ],
  xlab = "", pch = pch, col = col,
  ylab = "LRT statistic", cex.lab = 1, cex.axis = 1,
  cex = cex
)
mtext("d", side = 3, line = 1, adj = 0, cex = 1.3)


dev.off()
