# Look at zeros in daily and weekly cases

rm(list = ls())
graphics.off()

load("data/processed_long_dat.Rdata")

# Example of the potential "zeros problem" in daily data
# It looks as if cases alternate between 0s and nonzero numbers
# in a way that might be nonrandom
par(mfrow = c(2, 1))    
plot(new_cases[1, ])


# Convert to weekly data
new_cases_weekly <- matrix(NA, nrow(new_cases), ncol(new_cases))
for (i in seq_len(nrow(new_cases))) {

    new_cases_weekly[i, ] <- filter(new_cases[i, ], rep(1, 7), sides = 1)

}



# Check that it looks better
plot(new_cases_weekly[1, ])

