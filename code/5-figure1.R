rm(list = ls())

source("code/lrt.R")


load("data/processed/simulated_curves.Rdata")
load("data/processed/nyt_weekly.Rdata")
load("data/processed/theta_lg_pops.Rdata")
load("data/processed/pvals_sim_LRT.Rdata")


filename <- "figures/fig1.pdf"
pdf(filename)

par(mfrow = c(3, 2))

# Trim empirical incidence to same length as thetas2
series <- cases[10, 8:(length(cases[10, ]) - 8 + 1)]
dates <- dates[8:(length(cases[10, ]) - 8 + 1)]


# Row 1
# Define conditions for filtering curves
condition_a <- curve_parms$theta1 == curve_parms$theta2 &
  curve_parms$breakpoint == 30 &
  curve_parms$final_size == 1e6 &
  curve_parms$curve_type == 2 &
  curve_parms$theta1 == 1.1

condition_b <- curve_parms$theta1 == 10.1 &
  curve_parms$theta2 == 1.1 &
  curve_parms$breakpoint == 30 &
  curve_parms$final_size == 1e6 &
  curve_parms$curve_type == 2

# Plot for condition_a
plot(
  curves[which(condition_a), ],
  xlab = "Day", ylab = "Incidence", type = "l",
  cex.lab = 1.7, cex.axis = 1.7
)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.2)

# Plot for condition_b
plot(
  curves[which(condition_b), ],
  xlab = "Day", ylab = "Incidence", type = "l",
  cex.lab = 1.7, cex.axis = 1.7
)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.2)




# Row 2
# Extract repeated theta1 values for condition_a
theta1_values_a <- rep(
  curve_parms$theta1[which(condition_a)],
  times = length(curves[which(condition_a), ])
)

# Plot for condition_a
plot(
  theta1_values_a,
  xlab = "Day", ylab = expression(theta),
  col = 4, cex = 0.1, type = "l",
  cex.lab = 1.55, cex.axis = 1.45,
  ylim = c(0, 11)
)
mtext("c", side = 3, line = 1, adj = 0, cex = 1.2)
mtext("O", side = 3, line = -2, cex = 1.2)

# Extract repeated theta1 and theta2 values for condition_b
theta1_values_b <- rep(
  curve_parms$theta1[which(condition_b)],
  times = length(curves[which(condition_b), ]) / 2
)

theta2_values_b <- rep(
  curve_parms$theta2[which(condition_b)],
  times = length(curves[which(condition_b), ]) / 2
)

# Combine theta1 and theta2 values
theta_values_b <- c(theta1_values_b, theta2_values_b)

# Plot for condition_b
plot(
  theta_values_b,
  xlab = "Day", ylab = expression(theta),
  col = 4, cex = 0.1, type = "l",
  cex.lab = 1.55, cex.axis = 1.45,
  ylim = c(0, 11)
)
mtext("d", side = 3, line = 1, adj = 0, cex = 1.2)
mtext("X", side = 3, line = -2, cex = 1.2)

# Row 3
pops <- c(10000, 10000000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (i in seq_along(pops)) {
  current_dtheta <- dtheta[which(curve_parms$population == pops[i])]
  current_pvals <- pvals[which(curve_parms$population == pops[i])]

  df <- data.frame(adt = abs(current_dtheta), p = current_pvals)

  boxplot(p ~ adt,
    data = df,
    lty = 1,
    xlab = expression(paste(abs(theta[2] - theta[1]))),
    ylab = "p-value", col = "lightblue",
    range = 0
  )

  mtext(letters[5:6][i], side = 3, line = 1, adj = 0, cex = 1.2)

  text(1, 0.9, "O", cex = 1.2)
  text(10, 0.9, "X", cex = 1.2)
}

dev.off()
