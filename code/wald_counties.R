# Compute p-values for a state's counties
source("code/W.R")

filename <- "data/processed_dat.RData"
load(filename)
rm(filename)

# Loop through counties and calculate p values
pvals <- rep(NA, nrow(populations))

for (i in seq_len(nrow(populations))) {

  pvals[i] <- tryCatch(
    W(new_cases_subset[i, ],
      population_size = populations$population[i],
      breakpoint = 30, deg_free = 3,
      fn = my_spl_fit, verbose = FALSE
    ),
    error = function(e) {
      return(NA)
    }
  )

  cat("County", i, "of", nrow(populations), "p-value:", pvals[i], "\n")

}

# Save p-values
filename <- "data/pvals_allcounties.Rdata"
save(pvals, file = filename)


# Visualize p-values
hist(pvals, col = 2)
