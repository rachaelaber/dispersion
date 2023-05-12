# Visualize fitted curves in a state's counties where significant Wald result observed

ny_indeces <- which(ny_df_t$p < 0.05)

par(mar=c(1,1,1,1))
par(mfrow=c(6,2))

for (i in ny_indeces){
  
  mu <- diy_spl_fit(Y = ny_df_t$DailyNewConfirmedCases[[i]], population = ny_df_t$populations[i], inds=1:length(ny_df_t$DailyNewConfirmedCases[[i]]), df = 3)$mu
  
  plot(1:length(ny_df_t$DailyNewConfirmedCases[[i]]), ny_df_t$DailyNewConfirmedCases[[i]], ylim=c(0,500))
  lines(mu, col=2)
}

fl_indeces <- which(fl_df_t$p < 0.05)

par(mar=c(1,1,1,1))
par(mfrow=c(6,2))

for (i in fl_indeces){
  
  mu <- diy_spl_fit(Y = fl_df_t$DailyNewConfirmedCases[[i]], population = fl_df_t$populations[i], inds=1:length(fl_df_t$DailyNewConfirmedCases[[i]]), df = 3)$mu
  
  plot(1:length(fl_df_t$DailyNewConfirmedCases[[i]]), fl_df_t$DailyNewConfirmedCases[[i]])
  lines(mu, col=2)
}
