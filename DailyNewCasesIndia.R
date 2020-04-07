

library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
# library(plotly)
source('./source_crowd_data.r')
source('./source_official_data.r')
source('./source_world_data.R')

rebuild_dataframes = T

if (rebuild_dataframes) {
  dc <- build_crowd_data()
  dc_i <- dc$dc_i
  do_i <- build_official_data()
  d_i_wide <- rbind(do_i, dc_i)
}

data_i_new_cases <- d_i_wide  %>% select(StateUt, Date, Total, Source) %>% 
  group_by(Date, Source) %>%
  summarise(Total = sum(Total)) %>% group_by(Source) %>%
  mutate( TotalPrev = lag(Total, default = 0, order_by = Date)) %>%
  filter( Date > ymd('2020-03-03')	) %>%
  mutate( NewCases = (Total - TotalPrev) ) %>%
  ungroup()

p <- ggplot(
  data = data_i_new_cases,  
  aes(x = Date, y = NewCases, group = Source, color = Source, fill=Source)
  ) + 
  geom_vline(xintercept = dmy('24-03-2020'), linetype = '11' ) + 
  annotate(
    "text", x = dmy('25-03-2020'), y = 250, 
    size = 5, label = "Lockdown", hjust = 0, fontface = 1 ) +
  theme_minimal() +
  geom_line(linetype = 1, alpha = 0.8, size = 1) + 
  geom_point(alpha = 0.8, size = 3, stroke = 1, shape = 21, fill = 'white') + 
  # geom_smooth(alpha = 0.05, size=0.6, se = F) +
  labs(y = "Number of new cases reported")  + 
  # ylim(c(-10, 50)) +
  theme(legend.position = c(0.4, 0.6) ) +
  scale_fill_viridis_d(end = 0.6) + scale_color_viridis_d(end=0.6)

print(p)