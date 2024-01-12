# Visualize lrt p-values for counties

filename <- "figures/lrt_counties.pdf"

load("data/lrt_pvals_phis_allcounties.Rdata")
load("data/processed_dat.Rdata")

pdf(filename, width = 6, height = 6)

p_max <- 0.05

sig <- pvals < p_max

par(cex = 1.5)
par(pin = c(4, 4))

theta1 <- 1/phi11
theta2 <- 1/phi12

plot(log(theta1), log(theta2),
     cex = populations_subset$population/1000000,
     col = ifelse(sig, 2, grey(0.5)),
     xlab = expression(paste("log ", theta[1])),
     ylab = expression(paste("log ", theta[2])),
     #ylim = c(-1, 8),
     #xlim = c(-1, 8)
)

dev.off()

# Evaluate points that look like they should've been significant

# CLUSTER ONE

# which(log(theta1) < -30 & abs(log(theta2)) < 10)

# Index 88 - envelope not produced because spl is NA; lrt fine
#"Error in irls.nb.1(y, s, x, 1, beta0) : 0s in V(mu)"
# Index 536 - envelope not produced because spl is NA; lrt fine
#"Error in irls.nb.1(y, s, x, 1, beta0) : 0s in V(mu)"

# CLUSTER TWO

# which(log(theta2) > 10 & abs(log(theta1)) < 10)

# Index 204: significant, disregard
# Index 214: envelope not produced because
# Index 299: envelope produced but it looks like the case values nonsensical
# Index 404: significant, disregard
# Index 437: envelope not produced because
# Index 445: envelope not produced because
# Index 462: envelope not produced because
# Index 465: envelope not produced because
# Index 473: envelope produced but it looks like the case values nonsensical
# Index 506: envelope not produced because
# Index 668: envelope produced but it looks like the case values nonsensical
# Index 1612: envelope not produced because
# Index 1623: envelope produced but it looks like the case values nonsensical
# Index 1654: envelope produced but it looks like the case values nonsensical
# Index 1707: envelope produced but it looks like the case values nonsensical
# Index 1708: envelope not produced because
# Index 1731: envelope produced but it looks like the case values nonsensical
# Index 1735: envelope produced but it looks like the case values nonsensical
# Index 1748: envelope produced but it looks like the case values nonsensical
# Index 1801: envelope produced but case values mostly zero
# Index 1922: envelope produced but it looks like the case values nonsensical
# Index 1973: envelope produced but it looks like the case values nonsensical
# Index 2295: envelope not produced because
# Index 2359: envelope produced but it looks like the case values nonsensical
# Index 2380: envelope produced but it looks like the case values nonsensical
# Index 2393: envelope produced but it looks like the case values nonsensical
# Index 2415: envelope produced but it looks like the case values nonsensical



