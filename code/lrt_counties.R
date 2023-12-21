# Compute p-values for counties 

source("code/lrt.R")

filename <- "data/processed_dat.RData"

load(filename)

rm(filename)

# Loop through counties 

pvals <- rep(NA, nrow(populations_subset))
phi11 <- rep(NA, nrow(populations_subset))
phi12 <- rep(NA, nrow(populations_subset))
fips <- rep(NA, nrow(populations_subset))

for (i in 1:nrow(populations_subset)){
  pvals[i] <- tryCatch(lrt(y1 = new_cases_subset[i,][1:30], y2 = new_cases_subset[i,][31:60], 
                  s1 = populations_subset$population[i], s2 = populations_subset$population[i], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$p, error = function(e) return(NA))
  phi11[i] <- tryCatch(lrt(y1 = new_cases_subset[i,][1:30], y2 = new_cases_subset[i,][31:60], 
                  s1 = populations_subset$population[i], s2 = populations_subset$population[i], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$phi11, error = function(e) return(NA))
  phi12[i] <- tryCatch(lrt(y1 = new_cases_subset[i,][1:30], y2 = new_cases_subset[i,][31:60], 
                  s1 = populations_subset$population[i], s2 = populations_subset$population[i], i1 = 1:30, 
                  i2 = 31:60, df1 = 3, df2 = 3)$phi12, error = function(e) return(NA))
  fips[i] <- populations_subset$countyFIPS[i]
  
  cat("County", i, "of", nrow(populations_subset), "p-value:", pvals[i], "\n")
  
}

# Save 

filename <- "data/lrt_pvals_phis_allcounties.Rdata"

save(pvals, phi11, phi12, fips, file = filename)

# Visualize

filename <- "figures/lrt_counties.pdf"

pdf(filename, width = 6, height = 6)

p_max <- 0.05

sig <- pvals < p_max

par(cex = 1.5)
par(pin = c(4, 4))

theta1 <- 1/phi11
theta2 <- 1/phi12

plot(log(theta1), log(theta2),
     cex = ifelse(sig, 1, 0.5),
     col = ifelse(sig, 2, grey(0.5)),
     xlab = expression(paste("log ", theta[1])),
     ylab = expression(paste("log ", theta[2])),
     #ylim = c(-1, 8),
     #xlim = c(-1, 8)
)

dev.off()
