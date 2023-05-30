library(tidyverse)
library(changepoint)
source("code/functions_intro.R")

filename <- "data/processed_dat.Rdata"
load(filename)
rm(filename)

# Changepoint detection using cpt.mean
scaled_cases <- map(as.data.frame(t(new_cases_subset)), ~ as.vector(scale(.)))
ch_pts <- map(scaled_cases, ~ cpts(cpt.mean(., class = TRUE, penalty = "MBIC", 
                                            pen.value = 0, method = "PELT", param.estimates = TRUE)))
segments <- map2(.x = as.data.frame(t(new_cases_subset[which(sapply(ch_pts, length) != 0),])), 
                 .y = ch_pts[which(sapply(ch_pts, length) != 0)],
                 ~ get_segments(series = .x, changepoints = .y))
mean_pw <- map(segments, ~ get_mean_pw(.))

# Changepoint detection using cpt.meanvar 
ch_pts2 <- map(as.data.frame(t(new_cases_subset)), 
               ~ cpts(cpt.meanvar(., class = TRUE, penalty = "MBIC", pen.value = 30, 
                                  method = "PELT", test.stat = "Poisson", 
                                  param.estimates = TRUE, minseglen = 2))) 
n_ch_pts2 <- map(ch_pts2, ~length(unlist(.)))

# Plots
filename <- "figures/changepoint_figures.pdf"
pdf(filename, width = 6, height = 6)

plot(populations$population[which(sapply(ch_pts, length) != 0)], log(unlist(mean_pw)), 
     xlab="Log Population Size", ylab=" Log Mean PW Statistic", col=2, xlim=c(0, 1000000))

plot(populations$population, log(unlist(n_ch_pts2)), xlab="Log Population Size", 
     ylab=" Log Number of Changepoints", col=2)

dev.off()
