
library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
library(kableExtra)
# library(plotly)
source('./source_crowd_data.r')

if (F) {
  dc <- build_crowd_data()
  dc_i <- dc$dc_i
  dc_raw_i <- dc$dc_raw_i
  rm(dc)
  dc_age_i <- dc_raw_i %>% 
    mutate(AgeBracket = as.numeric(AgeBracket)) %>% 
    filter(!is.na(AgeBracket)) %>% 
    ungroup()
}



p <- ggplot(data = dc_age_i, aes(x = AgeBracket, fill=Status)) + theme_minimal() +
  geom_histogram(stat = "bin", position = "stack", bins = 40, color = 'white') +
  scale_fill_viridis_d()

print(p)