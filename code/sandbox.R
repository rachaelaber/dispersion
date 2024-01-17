rm(list = ls())
graphics.off()

library(viridis)


# Prep data
load("data/lrt_lg_pops_weekly.Rdata")
load("data/new_cases_lg_weekly.Rdata")
load("data/theta_lg_pops_weekly.Rdata")
load("data/lrtps_lg_pops_weekly.Rdata")
load("data/processed_long_dat.Rdata")  


# Trim elements or columns that will have NAs for theta and lrt
keep <- 30 : (length(dates) - 30 + 1)
dates <- dates[keep]
new_cases_lg_weekly <- new_cases_lg_weekly[, keep]



# Functions
clamp <- function(x, a, b) {

    y <- x
    y[y < a] <- a
    y[y > b] <- b

    return(y)

}


# Plot

filename <- "figures/roughdraft_surfaces.pdf"

pdf(filename, width = 8, height = 8)

i <- 100

par(mfrow = c(2, 2))

x <- colMeans(new_cases_lg_weekly)
plot(dates, x, type = 'l')


x <- clamp(thetas[i,], 0, 100)
plot(dates, x)

# reasoning for clamping:
# as theta increases much beyond 30, it is not practically different
# than poisson. (see figures/ecdfs.pdf)


image(t(clamp(thetas, 0, 100)), col = rev(viridis(32)))



#
image(t((lrt_ps)))
legend("topright", cex = 0.5, bg = "white", legend = seq(round(min(lrt_ps, na.rm = TRUE), 2), 
                                round(max(lrt_ps, na.rm = TRUE), 2), 
                                length = 11), 
       fill = heat.colors(10))

dev.off()