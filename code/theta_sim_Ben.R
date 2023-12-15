# Ben’s quick code to think about what 
# different values in theta mean:
  
# Simulate
  
mu <- 100
  
x30 <- rnbinom(n = 10000, mu = mu, size = 30)
  
x3 <- rnbinom(n = 10000, mu = mu, size = 3)
  
x.3 <- rnbinom(n = 10000, mu = mu, size = 0.3)
  
y <- rpois(n = 10000, lambda = mu)
  
filename <- "figures/ecdfs.pdf"

pdf(filename, width = 6, height = 6)  
  
# Plot CDFs
  
plot(ecdf(y), xlim = c(0, 500), main = "Empirical CDF", cex = 0.5)
  
plot(ecdf(x30), col = 2, pch = 21, cex = 0.5, add = T)
  
plot(ecdf(x3), col = 3, pch = 21, cex = 0.5, add = T)
  
plot(ecdf(x.3), col = 4, pch = 21, cex = 0.5, add = T)
  
legend('bottomright',
         
       pch = rep(21, 4),
         
       col = 1:4,
         
       legend = c("Poisson",
                    
                  "theta = 30",
                    
                  "theta = 3",
                    
                  "theta = 0.3"))

dev.off()