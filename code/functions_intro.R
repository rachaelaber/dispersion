# Divide a time series given a set of changepoints

get_segments <- function(series, changepoints){
  pts <- c(1,changepoints)
  segments <- rep(NA, times = length(pts)-1)
  for (i in 1:length(segments)){
    segments[i] <- list(series[pts[i]:(pts[i+1]-1)])
  }
  return(segments)
}

# Get (mean) pw stat. (could pass a known set of means) given list of vectors
get_mean_pw <- function(segments){  
  pws = rep(NA, length(segments))
  for (j in 1:length(segments)){
    segment = segments[[j]]
    n = length(segment)
    pws[j] = sum(segment^2)/mean(segment) - sum(segment)
  } 
  return(mean(pws, na.rm = TRUE))  
}

# Fit Poisson model to time series segment
spl_fit_pois <- function(Y, population, inds){ 
  y = Y[inds]
  pop_vec = rep(population, times=length(y))
  fit = glm(y ~ gam(Y[inds] ~ s(inds))$fitted.values + offset(log(pop_vec)), family=poisson(link=log))
  return(fit)
}