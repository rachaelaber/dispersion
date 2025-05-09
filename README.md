# Time-series modeling of epidemics in complex populations: detecting changes in incidence dispersion over time

## Data availability and processing
COVID-19 cumulative case data are from [The New York Times](https://github.com/nytimes/covid-19-data/) and county population sizes are from [The United States Census Bureau](https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/totals/). Cumulative cases for the largest three counties in each state were converted to weekly counts by keeping the last observation from each week and differenced to compute new cases. Note that missing values for new cases were all at the beginning of the pandemic and were imputed as zero. Approximately 0.24% of new cases were negative due to corrections in the cumulative data. These were also imputed as zero.

## Software requirements
NBPSeq R package (depends on qvalue package which can be obtained via bioconductor).
   







