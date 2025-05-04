rm(list = ls())
graphics.off()

library(boot)


# Prep data
load("data/processed/nyt_weekly.Rdata")
load("data/processed/theta_lg_pops.Rdata")


# Parameters
ww <- 8


# Trim elements or columns that will have NAs for theta and lrt
keep <- ww:(length(dates) - ww)
dates <- dates[keep]
cases <- cases[, keep]
incidence <- incidence[, keep]
rm(keep)



# Slide window Spearman's correlation between cases and theta
window_size <- 32
n_boot <- 1000 # Number of bootstrap replicates


## Preallocate
rho <- numeric(ncol(cases) - window_size + 1)
rho_lower <- numeric(length(rho))
rho_upper <- numeric(length(rho))
rho_pval <- numeric(length(rho))
rho_dates <- dates[seq(window_size, length(dates))]


## Function for bootstrapping
spearman_boot <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d[, 1], d[, 2], method = "spearman"))
}


# Loop over sliding windows
rho_seq <- seq_along(rho)
for (i in rho_seq) {
  print(paste(i, " of ", length(rho_seq)))
  idx <- i:(i + window_size - 1)

  # Prepare data for the current sliding window
  x <- as.vector(log10(cases[, idx]))
  y <- as.vector(log10(thetas[, idx]))
  valid <- is.finite(x) & is.finite(y)
  df_boot <- data.frame(x = x[valid], y = y[valid])

  # Compute Spearman correlation
  rho[i] <- cor(df_boot$x, df_boot$y, method = "spearman")

  # Bootstrap
  boot_out <- boot(df_boot, statistic = spearman_boot, R = n_boot)
  ci <- boot.ci(boot_out, type = "perc")
  rho_lower[i] <- ci$percent[4]
  rho_upper[i] <- ci$percent[5]
  rho_pval[i] <- mean(abs(boot_out$t) >= abs(rho[i]))
}

filename <- "S1_Fig.pdf"
pdf(filename)

# Plot with confidence intervals
plot(rho_dates, rho,
  type = "l", col = "blue", lwd = 2,
  ylim = c(-0.2, 0.5),
  xlab = "Date",
  ylab = expression("Spearman's" ~ rho) # nolint
)
polygon(c(rho_dates, rev(rho_dates)), c(rho_lower, rev(rho_upper)),
  col = adjustcolor("blue", alpha.f = 0.2), border = NA
)
abline(h = 0, lty = 1)

dev.off()