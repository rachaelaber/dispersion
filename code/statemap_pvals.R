# Map a state's p-values

https://cran.r-project.org/web/packages/tigris/readme/README.html

```{r, echo=F, fig.cap="P-values for each New York county resulting from the test of the one-sided upper alternative that time series dispersion is greater after Thanksgiving 2020"}

ny <- counties("New York")

p <- round(ny_df_t$p, digits=1)
p[is.na(p)] <- "na"
ny$p <- p

ggplot(ny) +
  geom_sf(aes(fill = p)) 

fl <- counties("Florida")

p <- round(fl_df_t$p, digits=1)
p[is.na(p)] <- "na"
fl$p <- p 

ggplot(fl) +
  geom_sf(aes(fill=p))

```