rm(list = ls())
graphics.off()



# Load raw epi data for each year and combine

# csvs are not on github and can be downloaded from
# https://github.com/nytimes/covid-19-data/us-counties-2020.csv
# https://github.com/nytimes/covid-19-data/us-counties-2021.csv
# https://github.com/nytimes/covid-19-data/us-counties-2022.csv
# https://github.com/nytimes/covid-19-data/us-counties-2023.csv

epi <- read.csv("data/raw/us-counties-2020.csv")

temp <- read.csv("data/raw/us-counties-2021.csv")
epi <- rbind(epi, temp)

temp <- read.csv("data/raw/us-counties-2022.csv")
epi <- rbind(epi, temp)

temp <- read.csv("data/raw/us-counties-2023.csv")
epi <- rbind(epi, temp)

rm(temp)


# Remove regions we will not analyze
# Alaska is excluded due to fips mismatches between census and epi data
excluded_regions <- c("Alaska",
                      "American Samoa",
                      "District of Columbia",
                      "Guam",
                      "Puerto Rico",
                      "Northern Mariana Islands",
                      "Virgin Islands")
epi <- epi[!epi$state %in% excluded_regions, ]
epi <- epi[!is.na(epi$fips), ]


# Convert date
epi$date <- as.Date(epi$date, format = "%Y-%m-%d")



# Load population data
# csv is not on github and can be downloaded from
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/totals/  #nolint
temp <- read.csv("data/raw/co-est2021-alldata.csv")
pop <- data.frame(fips = temp$STATE * 1000 + temp$COUNTY,
                  population = temp$POPESTIMATE2020,
                  state = temp$STNAME,
                  county = temp$CTYNAME)
pop <- pop[pop$state != pop$county, ] # remove state rows
rm(temp)



# Merge epi and pop size data
dat <- merge(epi, pop, by = "fips", all.x = TRUE)



# Investiage any unmatched fips
unmatched <- subset(dat, !complete.cases(dat))


# Drop unnecessary columns
dat <- data.frame(county = dat$county.x,
                  state = dat$state.x,
                  fips = dat$fips,
                  date = dat$date,
                  cases = dat$cases,
                  population = dat$population)
dat <- dat[order(dat$fips, dat$date), ]


# Subset to the three largest counties in each state
ustate <- unique(dat$state)
nstate <- length(ustate)

keep <- NULL

for (i in 1:nstate) {
  j <- which(dat$state == ustate[i])
  q <- sort(unique(dat$population[j]), decreasing = TRUE)[4]
  k <- which(dat$population[j] > q)
  keep <- c(keep, j[k])
}

dat_lg <- dat[keep, ]



# Assemble into matrices of county x day
# while also applying an imputation scheme as follows

ufips <- unique(dat_lg$fips)
nfips <- length(ufips)

date_seq <- seq.Date(from = min(dat_lg$date), to = max(dat_lg$date), by = "day")
day_seq <- weekdays(date_seq)

scaffold <- data.frame(date = date_seq)

cases <- matrix(NA, nrow = nfips, ncol = length(date_seq))
incidence <- matrix(NA, nrow = nfips, ncol = length(date_seq))
imputed <- matrix(FALSE, nrow = nfips, ncol = length(date_seq))
pops <- rep(NA, nfips)

for (i in 1:nfips) {
  # subset one fips
  ss <- subset(dat_lg, fips == ufips[i])
  pop <- ss$population[1]

  # merge to date scaffold and time order
  ss <- merge(scaffold, ss, all.x = TRUE)
  ss$population <- pop
  ss <- ss[order(ss$date), ]

  # mark rows for imputation
  # where reported cases are missing or nonincreasing
  x <- ss$cases # temp for calculating running max...
  x[is.na(x)] <- 0 # ...which will not ignore NA values
  ss$running_max <- cummax(x)
  ss$impute <- c(0, diff(ss$running_max)) == 0


  # the imputation is linear interpolation
  # between non-impute values and will return
  # the observation for non-impute rows
  ss0 <- ss[!ss$impute, ]
  f <- approxfun(ss0$date, ss0$cases)


  # apply to make predictions for every date
  # extrapolating 0 prior to first reported case
  ss$cases_pred <- f(ss$date)
  ss$cases_pred[is.na(ss$cases_pred)] <- ss$running_max[is.na(ss$cases_pred)]
  before <- 1:(which.min(ss$cases) - 1)
  ss$cases_pred[before] <- 0


  # imputed new cases are poisson distributed
  # around the imputation prediction
  lambda <- c(0, diff(ss$cases_pred))
  ss$new_cases <- rpois(length(lambda), lambda)


  # write to matrices, now calling new cases "cases"
  cases[i, ] <- ss$new_cases
  incidence[i, ] <- ss$new_cases / ss$population
  imputed[i, ] <- ss$impute
  pops[i] <- ss$population[1]
}


# Assemble a data frame of county info
county <- dat_lg[!duplicated(dat_lg$fips),
                 c("fips", "county", "state", "population")]


# Save
filename <- "data/processed/nyt.Rdata"
save(cases, incidence, imputed, pops, county, date_seq,
     file = filename)