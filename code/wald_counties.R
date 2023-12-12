# Compute p-values for a state's counties
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p values
pvals <- rep(NA, nrow(populations_subset))
ltheta1 <- rep(NA, nrow(populations_subset))
ltheta2 <- rep(NA, nrow(populations_subset))
fips <- rep(NA, nrow(populations_subset))

for (i in seq_len(nrow(populations_subset))) {

    pvals[i] <- tryCatch(W(new_cases_subset[i, ],
                  population_size = populations_subset$population[i],
                  breakpoint = 30,
                  deg_free = 3,
                  fn = my_spl_fit),
                  error = function(e) return(NA))
    ltheta1[i] <- tryCatch(W(new_cases_subset[i, ],
                   population_size = populations_subset$population[i],
                   breakpoint = 30,
                   deg_free = 3,
                   fn = my_spl_fit,
                   ret_lthetas = TRUE)[[1]][1],
                   error = function(e) return(NA))
    ltheta2[i] <- tryCatch(W(new_cases_subset[i, ],
                             population_size = populations_subset$population[i],
                             breakpoint = 30,
                             deg_free = 3,
                             fn = my_spl_fit,
                             ret_lthetas = TRUE)[[1]][2],
                           error = function(e) return(NA))

     fips[i] <- populations_subset$countyFIPS[i]

  cat("County", i, "of", nrow(populations_subset), "p-value:", pvals[i], "\n")
}

# Save p-values
filename <- "data/W_pvals_lthetas_allcounties.Rdata"
save(pvals, ltheta1, ltheta2, fips, file = filename)

# Visualize thetas and p-values

filename <- "figures/wald_results.pdf"

pdf(filename, width = 6, height = 6)

p_max <- 0.05

sig <- pvals < p_max

par(cex = 1.5)
par(pin = c(4, 4))

plot(ltheta1, ltheta2,
  cex = ifelse(sig, 1, 0.5),
  col = ifelse(sig, 2, grey(0.5)),
  xlab = expression(paste("log ", theta[1])),
  ylab = expression(paste("log ", theta[2]))
)

dev.off()