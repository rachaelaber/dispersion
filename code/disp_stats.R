# ```{r}
# disp_stats <- c()
# for (i in 1:dim(new_cases_subset[state_index])[1]){
#   Y <- new_cases_subset[i,]
#   spline_mod.1.pois <- spl_fit_pois(Y = Y, population=ny_fl_df_t$populations[i], inds=1:(length(Y)/2))
#   disp_stats <- c(disp_stats, spline_mod.1.pois$deviance/spline_mod.1.pois$df.residual)
# }
# `