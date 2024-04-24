rm(list = ls())

# Raw COVID-19 data  
filename <- "data/raw/covid_confirmed_usafacts.csv"
dat <- read.csv(filename, stringsAsFactors = FALSE)

# Remove statewide unallocated
dat <- dat[dat$countyFIPS != 0,]

# Population data  
filename <- "data/raw/covid_county_population_usafacts.csv"
populations <- read.csv(filename, stringsAsFactors = FALSE)

# Remove statewide unallocated
populations <- populations[populations$countyFIPS != 0,]

# Add population column to dat (note that row 96 ("Wade Hampton Census Area") and 193
# ("Grand Princess Cruise Ship") of the population data aren't represented in case data)
populations <- populations[which(populations$countyFIPS %in% dat$countyFIPS),]

# Break up variables
population <- populations$population
countyFIPS <- dat$countyFIPS
county_name <- dat$County.Name
state <- dat$State
stateFIPS <- dat$StateFIPS
population <- dat$population

# New cases are estimated by differencing cumulative cases
cumulative_cases <- as.matrix(dat[, 5:ncol(dat)])
new_cases <- matrix(NA, nrow(cumulative_cases), ncol(cumulative_cases))
for (i in 1:nrow(cumulative_cases)) {

    new_cases[i, ] <- c(0, diff(cumulative_cases[i, ]))

}

dates <- names(dat)[5:ncol(dat)]

# Process dates
dates <- gsub("X", "", dates)
dates <- as.Date(dates, format = "%Y.%m.%d")

# Clamp new cases to be at least zero
new_cases[which(new_cases < 0)] <- 0

# Remove time series that are all = zero
new_cases <- new_cases[which(rowSums(new_cases) != 0),]
populations <- populations[which(rowSums(new_cases) != 0),]

# Save full data 
filename <- "data/processed/processed_long_dat.Rdata"
save(new_cases, populations, dates, file = filename)

# Create weekly data 
new_cases_weekly <- matrix(NA, nrow(new_cases), ncol(new_cases))

for (i in seq_len(nrow(new_cases))) {
  
  new_cases_weekly[i, ] <- filter(new_cases[i, ], rep(1, 7), sides = 1)
  
}

# Replace NAs with 0s
new_cases_weekly[is.na(new_cases_weekly)] <- 0

# Save weekly data
filename <- "data/processed/new_cases_weekly.Rdata"

save(new_cases_weekly, file = filename)

# Subset weekly data to be just large counties
unique_states <- unique(populations$State)

nstate <- length(unique_states)

keep <- c()

for (i in 1:nstate) {
  
  j <- which(populations$State == unique_states[i])
  
  k <- which(populations$population[j] > quantile(populations$population[j], 0.96))
  
  keep <- c(keep, j[k])
}

new_cases_lg_weekly <- new_cases_weekly[keep, ]

populations_lg <- populations[keep, ]

# Save
filename <- "data/processed/new_cases_lg_weekly.Rdata"

save(new_cases_lg_weekly, populations_lg, file = filename)