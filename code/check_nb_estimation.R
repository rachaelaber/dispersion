load("data/simulated_curves.Rdata")
source("code/W.R")

i <- 5000
Y <- curves[i, ]
population <- curve_parms$population[i]

out <- my_spl_fit(Y = Y, population = population, inds = 1:30, df = 3)

# Check results visually
ylim <- range(curves)
plot(Y, ylim = ylim)
lines(out$mu, col = "red", lwd = 2)