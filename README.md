# State wise timeseries data of covid19 cases in India

The data is compiled manually using the daily updates about the outbreak on Ministry of Health and Family Welfare site: https://www.mohfw.gov.in/

Will try to keep it as updated as possible. Please feel free point out any mistakes. 

Contributions welcome 

### How to read the data in R

``` R
library(readr)
d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")
```
### Script
1. Clone/Fork/Download the repo
2. Change the working directory path at the top of the script
3. Change `animate` switch to `T` if you want animation output, `F` if you want the plot of yesterdays data.
4. Run the script

### Output Animation 

![Output Animation](https://github.com/rahulnyk/COVID19_IndiaData/blob/master/output.gif)

### Output Plot 

![Total cases plot](https://github.com/rahulnyk/COVID19_IndiaData/blob/master/output.jpg)

### Growth rate output plot

![Growth rate plot](https://github.com/rahulnyk/COVID19_IndiaData/blob/master/gr_output.jepg)
