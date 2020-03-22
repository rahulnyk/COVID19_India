# State wise timeseries data of covid19 cases in India

The data is compiled manually using the daily updates about the outbreak on Ministry of Health and Family Welfare site: https://www.mohfw.gov.in/

Contributions welcome 

### How to read the data in R

``` R
library(readr)
d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")
```