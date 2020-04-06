## R Markdown dashboard for COVID19 India Data. 
This repo is use to host R dashboard which tracks COVID India data. The dashboard can be accessed throug the following link .

https://rahulnyk.github.io/COVID19_India/

I am in the process of consolidating the dashboard so there may be some temporary mess in the repo. Will clean it up as soon as possile :)

** Contributions most welcome ** 


# State wise timeseries data of covid19 cases in India

**NOTE: In an effort to consolidate the data, we have moved it to the new repo **

https://github.com/rahulnyk/covid19_india_data

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

![Growth rate plot](https://github.com/rahulnyk/COVID19_IndiaData/blob/master/gr_output.jpeg)
