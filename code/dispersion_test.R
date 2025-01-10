# Wrapper for lrt
# expects length(y) to be even

dispersion_test <- function(y, s, df, ptol, ztol) {
  ww <- length(y) / 2

  i1 <- 1:ww
  i2 <- (ww + 1):(2 * ww)

  y1 <- y[i1]
  y2 <- y[i2]

  out <- tryCatch(lrt( # nolint
    y1 = y1,
    y2 = y2,
    s1 = s,
    s2 = s,
    i1 = i1,
    i2 = i2,
    df1 = df,
    df2 = df,
    ptol = ptol,
    ztol = ztol
  ), error = function(e) {
    return(NA)
  })

  if (!is.list(out)) {
    theta0_est <- NA
    theta1_est <- NA
    theta2_est <- NA
    res01 <- NA
    res02 <- NA
    res11 <- NA
    res22 <- NA
    lambda <- NA
    p <- NA
    fail_to_reject_poisson <- NA
    collapse_to_zero <- NA
  } else {
    theta0_est <- 1 / out$phi0
    theta1_est <- 1 / out$phi11
    theta2_est <- 1 / out$phi12
    res01 <- out$res01
    res02 <- out$res02
    res11 <- out$res11
    res22 <- out$res22
    lambda <- out$lambda
    p <- out$p
    fail_to_reject_poisson <- out$fail_to_reject_poisson
    collapse_to_zero <- out$collapse_to_zero
  }

  return(list(
    theta0_est = theta0_est,
    theta1_est = theta1_est,
    theta2_est = theta2_est,
    res01 = res01,
    res02 = res02,
    res11 = res11,
    res22 = res22,
    lambda = lambda,
    p = p,
    fail_to_reject_poisson = fail_to_reject_poisson,
    collapse_to_zero = collapse_to_zero
  ))
}
