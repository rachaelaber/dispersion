load("./data/processed_dat.Rdata")
load("./data/simulated_curves.Rdata")

filename <- "figures/empirical_increase.pdf"

pdf(filename, width = 6, height = 6)

american_thanksgiving <- as.Date("2020-11-26")
start_date <- american_thanksgiving - 29
end_date <- american_thanksgiving + 30
days <- seq(start_date, end_date, by = "day")

par(mfrow = c(1,2))
plot(new_cases_subset[1842,] ~ days,  ylab="Daily Incidence", xlab="Date") 
abline(v = american_thanksgiving, col=2)
plot(curves[10000,] ~ days, ylab="Daily Incidence", xlab="Date") # theta_1 = theta_2 = 12
abline(v = american_thanksgiving, col=2)

dev.off()
