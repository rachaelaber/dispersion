rm(list = ls())
graphics.off()



# Load raw epi data for each year and combine
# Note cases is cumulative

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


# Convert dates
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


# Investigate unmatched fips
unmatched <- subset(dat, !complete.cases(dat))


# Keep needed columns
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


# Assemble into matrices of county x week
ufips <- unique(dat_lg$fips)
nfips <- length(ufips)

startdate <- as.Date("2020-01-01")
enddate <- max(dat_lg$date)
dates <- seq.Date(from = startdate, to = enddate, by = "day")
nweek <- sum(weekdays(dates) == "Saturday")

cases <- matrix(NA, nrow = nfips, ncol = nweek)
incidence <- matrix(NA, nrow = nfips, ncol = nweek)
pops <- rep(NA, nfips)

scaffold <- data.frame(date = dates)

for (i in 1:nfips) {

  # subset one fips
  ss <- subset(dat_lg, fips == ufips[i])
  pop <- ss$population[1]


  # merge to date scaffold and time order
  ss <- merge(scaffold, ss, all.x = TRUE)
  ss$population <- pop
  ss <- ss[order(ss$date), ]


  # to go from daily to weekly with cumulative cases
  # keep the last observation each week
  ss <- ss[weekdays(ss$date) == "Saturday", ]


  # calculate new cases
  ss$new_cases <- c(NA, diff(ss$cases))


  # write to matrices, now calling new cases "cases"
  cases[i, ] <- ss$new_cases
  incidence[i, ] <- ss$new_cases/ss$population
  pops[i] <- ss$population[1]
}


# Assemble a data frame of county info
county <- dat_lg[!duplicated(dat_lg$fips),
                 c("fips", "county", "state", "population")]


# Dates to weekly
dates <- dates[weekdays(dates) == "Saturday"]


# Missing values for cases are all at the beginning
# of the pandemic and are imputed as 0
cases[is.na(cases)] <- 0
incidence[is.na(incidence)] <- 0


# Apprxoimately 0.24% of new cases are negative
# due to corrections in the cumulative data.
# These are imputed as 0
cases[cases < 0] <- 0
incidence[incidence < 0] <- 0



# Save
filename <- "data/processed/nyt_weekly.Rdata"
save(cases, incidence, pops, county, dates,
     file = filename)
