# Generate (noisy) epidemic curves with known parameters
# to test the validity and power of the proposed method

rm(list = ls())

source("code/curve_templates.R")
load("data/processed/new_cases_lg_weekly.Rdata")
populations <- populations_lg$population
rm(populations_lg)
rm(new_cases_lg_weekly)


# Parameters
final_attack_rate <- 0.1
peak_time <- 30
peak_width <- 200
population_min <- 10000
population_mid <- signif(min(populations), 2)
population_max <- signif(max(populations), 2)
theta_min <- 3
theta_max <- 30
breakpoint_min <- 20
breakpoint_max <- 40

ntheta <- 10      # Number of unique theta values
nbreakpoint <- 5  # Number of unique breakpoint values

nstep <- 60       # Number of time steps (days) in simulated trajectories

# Theta, breakpoint, population size, and curve type sequences
theta1 <- seq(theta_min, theta_max, length.out = ntheta)
theta2 <- seq(theta_min, theta_max, length.out = ntheta)
breakpoint <- seq(breakpoint_min, breakpoint_max, length.out = nbreakpoint)
population <- c(population_min, population_mid, population_max)
curve_type <- c(1, 2)

# Expand grid and add final size
curve_parms <- expand.grid(theta1 = theta1,
                           theta2 = theta2,
                           breakpoint = breakpoint,
                           population = population,
                           curve_type = curve_type)

curve_parms$final_size <- final_attack_rate * curve_parms$population

rm(theta1, theta2, breakpoint, population, curve_type)

# Generate epidemic curves
ncurve <- nrow(curve_parms)
curves <- matrix(NA, nrow = ncurve, ncol = nstep)
for (i in 1:ncurve) {

    breakpoint <- curve_parms$breakpoint[i]
    theta1 <- curve_parms$theta1[i]
    theta2 <- curve_parms$theta2[i]
    final_size <- curve_parms$final_size[i]
    curve_type <- curve_parms$curve_type[i]

    means <- get_template(param = c(final_size, peak_time, peak_width, nstep),
                          type = curve_type)

    means1 <- means[1:breakpoint]
    means2 <- means[(breakpoint + 1):length(means)]

    k1 <- theta1
    k2 <- theta2

    c1 <- rnbinom(n = length(means1), mu = means1, size = k1) 
    c2 <- rnbinom(n = length(means2), mu = means2, size = k2) 

    curves[i, ] <- c(c1, c2)

}

# Save curves
filename <- "data/processed/simulated_curves.Rdata"
save(curves, curve_parms, file = filename)
