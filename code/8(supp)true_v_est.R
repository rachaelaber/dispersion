source("code/lrt.R")


load("data/processed/simulated_curves.Rdata")


inds <- which(curve_parms$theta1 == curve_parms$theta2) # indices where true theta remains the same
curves <- curves[inds, ] # subset curves by those indices
ncurve <- nrow(curves)
curve_parms <- curve_parms[inds, ]


phi0s <- rep(NA, length(inds))

for (i in 1:nrow(curves)) {
  phi0s[i] <- tryCatch(lrt(
    y1 = curves[i, ][(curve_parms$breakpoint[i]-7):curve_parms$breakpoint[i]],
    y2 = curves[i, ][(curve_parms$breakpoint[i]+1):(curve_parms$breakpoint[i] + 8)],
    s1 = curve_parms$population[i],
    s2 = curve_parms$population[i],
    i1 = 1:8,
    i2 = 9:16,
    df1 = 3,
    df2 = 3
  )$phi0, error = function(e) return(NA))
  
  if (i %% 100 == 0) {
    cat(i, "of", ncurve, "\n")
  }
}


theta0s <- 1/phi0s
true_theta <- curve_parms$theta1

filename <- "figures/thetaest_v_theta.pdf"
pdf(filename)

plot(log10(true_theta), log10(theta0s), xlab = expression(log[10](theta[true])), ylab = expression(log[10](theta[est])),
     col = ifelse(curve_parms$population < 50000, "red", "black"))
abline(0, 1, col = "red")

dev.off()