# COVID data  
filename <- "data/covid_confirmed_usafacts.csv"
dat <- read.csv(filename, stringsAsFactors = FALSE)

# Remove statewide unallocated
dat <- dat[dat$countyFIPS != 0,]

# Population data  
filename <- "data/covid_county_population_usafacts.csv"
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

cumulative_cases <- as.matrix(dat[, 5:ncol(dat)])
new_cases <- matrix(NA, nrow(cumulative_cases), ncol(cumulative_cases))
for (i in 1:nrow(cumulative_cases)) {

    new_cases[i, ] <- c(0, diff(cumulative_cases[i, ]))

}

dates <- names(dat)[5:ncol(dat)]

# Process dates
dates <- gsub("X", "", dates)
dates <- as.Date(dates, format = "%Y.%m.%d")

# Subset to 60 days centered on Thanksgiving
canadian_thanksgiving <- as.Date("2020-10-12")
american_thanksgiving <- as.Date("2020-11-26")

start_date <- american_thanksgiving - 29
end_date <- american_thanksgiving + 30

dates_subset <- dates[dates >= start_date & dates <= end_date]
new_cases_subset <- new_cases[, dates >= start_date & dates <= end_date]
new_cases_subset[which(new_cases_subset < 0)] <- 0

new_cases_subset <- new_cases_subset[which(rowSums(new_cases_subset) != 0),]
populations <- populations[which(rowSums(new_cases_subset) != 0),]

# Save curves
filename <- "data/processed_dat.Rdata"
save(populations, new_cases_subset, file = filename)
