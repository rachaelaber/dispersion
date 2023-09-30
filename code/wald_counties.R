# Compute p-values for a state's counties
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p values
pvals <- rep(NA, nrow(populations_subset))
theta1 <- rep(NA, nrow(populations_subset))
theta2 <- rep(NA, nrow(populations_subset))
fips <- rep(NA, nrow(populations_subset))

for (i in seq_len(nrow(populations_subset))) {

    pvals[i] <- tryCatch(W(new_cases_subset[i, ],
                  population_size = populations_subset$population[i],
                  breakpoint = 30,
                  deg_free = 3,
                  fn = my_spl_fit), 
                  error = function(e) return(NA))
    theta1[i] <- tryCatch(W(new_cases_subset[i, ],
                   population_size = populations_subset$population[i],
                   breakpoint = 30,
                   deg_free = 3,
                   fn = my_spl_fit,
                   return_thetas = TRUE)[[1]][1], 
                   error = function(e) return(NA))
    theta2[i] <- tryCatch(W(new_cases_subset[i, ],
                   population_size = populations_subset$population[i],
                   breakpoint = 30,
                   deg_free = 3,
                   fn = my_spl_fit, return_thetas = TRUE)[[1]][2], 
                    error = function(e) return(NA))
   
     fips[i] <- populations_subset$countyFIPS[i]

  cat("County", i, "of", nrow(populations_subset), "p-value:", pvals[i], "\n")
}

# Save p-values
filename <- "data/W_pvals_and_thetas_allcounties.Rdata"
save(pvals, fips, theta1, theta2, file = filename)

# Visualize thetas and p-values

theta_max <- 100
p_max <- 0.05

keep <- theta1 < theta_max & theta2 < theta_max

sig <- pvals[keep] < p_max

par(cex = 1.5)
par(pin = c(4, 4))

plot(log(theta1[keep]), log(theta2[keep]),
  cex = ifelse(sig, 1, 0.5),
  col = ifelse(sig, 2, grey(0.5)),
  xlab = expression(paste("log ", theta[1])),
  ylab = expression(paste("log ", theta[2]))
)
