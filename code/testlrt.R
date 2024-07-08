
# Test lrt()

theta1 <- 3
theta2 <- 30
means <- rnorm(100, mean = 10, sd = 1)
dat1 <- rnbinom(n = length(means), mu = means, size = theta1) 
dat2 <- rnbinom(n = length(means), mu = means, size = theta2) 

phi01 <- lrt(y1 = dat1[1:50], y2 = dat1[51:100], s1 = 10000, s2 = 10000, i1 = 1:50, i2 = 51:100, df1 = 3, df2 = 3)$phi0
theta1 <- 1/phi01
theta1

phi02 <- lrt(y1 = dat2[1:50], y2 = dat2[51:100], s1 = 10000, s2 = 10000, i1 = 1:50, i2 = 51:100, df1 = 3, df2 = 3)$phi0
theta2 <- 1/phi02
theta2

