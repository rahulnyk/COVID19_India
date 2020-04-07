library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
library(kableExtra)
library(gganimate)
library(animation)
library(gifski)
library(plotly)
# library(plotly)
source('./source_crowd_data.r')
source('./source_official_data.r')


if(F) {
  dc <- build_crowd_data()
  dc_raw_i <- dc$dc_raw_i 
}
dc_raw_total_i <- dc_raw_i %>% 
  select(City, StateUt) %>% group_by(StateUt) %>%
  summarise(NumCities = n_distinct(City), Total = n() ) %>%
  filter(Total > 50)

p <- ggplot(data = dc_raw_total_i, aes(x = Total, y = NumCities, color = StateUt, size = Total)) +
  geom_point() + 
  geom_text(aes(label = StateUt), hjust = 1, angle = -45, size = 4) +
  theme_minimal() +
  theme(
    legend.position = 'off'
  ) 

print(p)



