load("./data/processed_dat.Rdata")

load("./data/simulated_curves.Rdata")

filename <- "figures/empirical_increase.pdf"

pdf(filename, width = 6, height = 6)

american_thanksgiving <- as.Date("2020-11-26")

start_date <- american_thanksgiving - 29

end_date <- american_thanksgiving + 30

days <- seq(start_date, end_date, by = "day")

par(mfrow = c(2,2))

plot(new_cases_subset[1862, ] ~ days,  ylab = "Daily Incidence in FIPS 36067 (Wald test p-value = 0.017)", 
     xlab = "Date", ylim = c(0, 1400)) 
abline(v = american_thanksgiving, col = 2)

plot(new_cases_subset[1863, ] ~ days, ylab = "Daily Incidence in FIPS 36069 (Wald test p_value = 0.099)", 
     xlab ="Date", ylim = c(0, 1400)) 
abline(v = american_thanksgiving, col = 2)

plot(curves[220, ] ~ days, ylab = "Simulated Daily Incidence (Wald test p-value = 0.004)", 
     xlab = "Date", ylim = c(0, 1400))
abline(v = american_thanksgiving, col = 2)

plot(curves[212, ] ~ days, ylab = "Simulated Daily Incidence (Wald test p-value = 0.260)", 
     xlab = "Date", ylim = c(0, 1400))
abline(v = american_thanksgiving, col = 2)

dev.off()
