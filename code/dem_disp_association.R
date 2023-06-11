# Assess associates between dtheta_clamped and demographic variables
library(stringi)


# Format FIPS in unemp data

data(unemp, package = "viridis")

statefips <-stri_pad_left(unemp$state_fips, 2, 0)

countyfips <- sprintf("%03d", unemp$county_fips)

unemp$fips <- paste(statefips, countyfips, sep = "")

# Format dtheta data

load("data/W_pvals_and_thetas_allcounties.Rdata")

fips <- stri_pad_left(fips, 5, 0)

dtheta <- theta1 - theta2

clamp <- function(x, c) {
  x[x > c] <- c
  x[x < -c] <- -c
  return(x)
}

dtheta_clamped <- clamp(dtheta, 20)

dat <- data.frame(fips = fips, dtheta_clamped = dtheta_clamped)

# Merge datasets

reg_df <- merge(unemp, dat, by = "fips")

# Regression

