
library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
library(kableExtra)
# library(plotly)
source('./source_crowd_data.r')
source('./source_official_data.r')
if (T) {
  dc <- build_crowd_data()
  dc_i <- dc$dc_i
  
  do_i <- build_official_data()
  d_i_wide <- rbind(do_i, dc_i)
  d_i_long <- d_i_wide %>% pivot_longer(
    -c(StateUt, Date, Source), 
    values_to = 'Cumulative', 
    names_to = 'Status')

d_i_long_total <- d_i_long %>% 
  filter(Status == 'Total') %>% 
  filter(Date == max(Date)) %>%
  filter(Cumulative > 5) %>%
  filter(!(StateUt == 'unassigned'))

d_i_long_total$StateUt[d_i_long_total$StateUt == "Telengana"] <- "Telangana"
}
p <- ggplot(data = d_i_long_total, aes(x = reorder( StateUt, Cumulative ), y = Cumulative, fill = Source)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  theme(
    legend.position = c(0.4, 0.8),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
    ) + scale_fill_viridis_d(option = 'cividis', end = 0.9)

print(p)
  
  