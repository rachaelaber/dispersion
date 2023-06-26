# Generate epidemic curve according to method in Box 2 of Grenfell et al. 2001
# to test the validity and power of the proposed method

rm(list = ls())

nstep <- 60

n <- 1:nstep

theta <- rep(NA, times = nstep)

lam <- rep(NA, times = nstep)

I <- rep(NA, times = nstep)

S <- rep(NA, times = nstep)

m <- 0.2 # ?

B <- 2 # ?

a <- 0.5 # ? 


for (i in n){
  
  theta[i] <- rpois(m * I[i])
  
  lam[i + 1] <- B[i] * S[i] * (I[i] + theta[i])^a   # Epidemic intensity
  
  I[i + 1] <- rnbinom(lam[i + 1], I[i])    # Number infected
  
  S[i + 1] <- S[i] + B - I[i + 1]
  
  
}

# Save curve
filename <- "data/simulated_curve_Grenfell.Rdata"
save(curve, file = filename)