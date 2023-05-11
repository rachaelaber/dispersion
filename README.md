# Detecting changes in dispersion in COVID-19 incidence time series using a negative binomial model




## Data
COVID case data and county population sizes are from [here](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/) the original versions of which are stored as [data/covid_confirmed_usafacts.csv](data/covid_confirmed_usafacts.csv) [data/covid_county_population_usafacts.csv](data/covid_county_population_usafacts.csv)



## Results outline

1. Validity and power of the method 
2. Testing and mapping "the Thanksgiving hypothesis"
3. Scanning for breakpoints and spatiotemporal mapping


## Methods

## Software requirements
Need NBPSeq package which depends on qvalue package which can be obtained via bioconductor.


### Validity and power of the method
Generate (noisy) epidemic curves with known parameters to test the validity and power of the proposed method ([code](code/generate_time_series.R)). Generates simulated curves stored [here](data/simulated_curves.Rdata).

### Testing and mapping "the Thanksgiving hypothesis"


### Scanning for breakpoints and spatiotemporal mapping
