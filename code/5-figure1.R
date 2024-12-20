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

plot(
  curves[which(curve_parms$theta1 == curve_parms$theta2 & curve_parms$breakpoint == 30 &               #nolint 
    curve_parms$final_size == 1000000 & curve_parms$curve_type == 2 & curve_parms$theta1 == 1.1), ],   #nolint 
  xlab = "Day", type = "l", ylab = "Incidence", cex.lab = 1.7, cex.axis = 1.7
)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.2)
plot(
  curves[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30 & #nolint 
    curve_parms$final_size == 1000000 & curve_parms$curve_type == 2), ],                               #nolint 
  xlab = "Day", type = "l", ylab = "Incidence", cex.lab = 1.7, cex.axis = 1.7
)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.2)



# Row 2
plot(rep(curve_parms$theta1[which(curve_parms$theta1 == curve_parms$theta2 & curve_parms$breakpoint == 30 & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2 & curve_parms$theta1 == 1.1)], 
         times = length(curves[which(curve_parms$theta1 == curve_parms$theta2 & curve_parms$breakpoint == 30 & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2 & curve_parms$theta1 == 1.1),])),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l", cex.lab = 1.55, cex.axis = 1.45, ylim = c(0, 11))

mtext("c", side = 3, line = 1, adj = 0, cex = 1.2)
mtext("O", side = 3, line = -2, cex = 1.2)

plot(c(rep(curve_parms$theta1[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30 & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2)], 
           times = length(curves[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30 & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2),])/2), 
       rep(curve_parms$theta2[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30 & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2)], 
           times = length(curves[which(curve_parms$theta1 == 1.1 & curve_parms$theta2 == 10.1 & curve_parms$breakpoint == 30  & curve_parms$final_size == 1000000 & curve_parms$curve_type == 2),])/2)),
     xlab = "Day", ylab = expression(theta), col = 4, cex = .1, type = "l", cex.lab = 1.55, cex.axis = 1.45, ylim = c(0, 11))

mtext("d", side = 3, line = 1, adj = 0, cex = 1.2)
mtext("X", side = 3, line = -2, cex = 1.2)

# Row 3
pops <- c(50000, 10000000)
dtheta <- curve_parms$theta2 - curve_parms$theta1

for (i in seq_along(pops)) {
  current_dtheta <- dtheta[which(curve_parms$population == pops[i])]
  current_pvals <- pvals[which(curve_parms$population == pops[i])]

  df <- data.frame(adt = abs(current_dtheta), p = current_pvals)

  boxplot(p ~ adt,
    data = df, #subset = adt <= 9,
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
