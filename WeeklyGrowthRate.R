

library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
# library(plotly)
source('./source_crowd_data.r')
source('./source_official_data.r')
source('./source_world_data.R')

rebuild_dataframes = F

if (rebuild_dataframes) {
  dc <- build_crowd_data()
  dc_i <- dc$dc_i
  do_i <- build_official_data()
  d_i_wide <- rbind(do_i, dc_i)
}
# 
# dweek <- d_i_wide %>% 
#   filter( Date > ymd('2020-03-01')	) %>%
#   mutate(week = lubridate::isoweek(Date)) %>% 
#   group_by(week) %>% 
#   mutate(WeekEnd = max(Date)) %>% 
#   ungroup() %>% 
#   select(WeekEnd, Total, Source) %>%
#   group_by(WeekEnd, Source) %>%
#   summarise(Total = sum(Total)) %>% 
#   ungroup() %>%
#   group_by(Source) %>% 
#   mutate( TotalPrev = lag(Total, default = 0, order_by = WeekEnd) ) %>%
#   mutate( NewCases = (Total - TotalPrev) ) %>%
#   mutate( WeekEndPrev = lag(WeekEnd, default = 0, order_by = WeekEnd) ) %>%
#   mutate( WeekCurrentPrev = as.integer(WeekEnd - WeekEndPrev) ) %>%
#   mutate( WeeklyMean = NewCases*100/(WeekCurrentPrev*TotalPrev) ) %>%
#   filter(!is.infinite(WeeklyMean)) %>%
#   select(WeekEnd, Source, NewCases, WeeklyMean) %>%
#   pivot_longer(-c(WeekEnd, Source), names_to = 'Metric', values_to = 'Value')
#   

## Calculating Previous sunday

d <-  Sys.Date()
prev.days <- seq(d-6,d,by='day')
PrevSunday <- prev.days[weekdays(prev.days)=='Sunday']

dweek <- d_i_wide  %>% select(StateUt, Date, Total, Source) %>% 
  group_by(Date, Source) %>%
  summarise(Total = sum(Total)) %>% group_by(Source) %>%
  mutate( TotalPrev = lag(Total, default = 0, order_by = Date)) %>%
  filter( Date > ymd('2020-03-01')	) %>%
  mutate( NewCases = (Total - TotalPrev) ) %>%
  ungroup() %>%
  mutate(week = lubridate::isoweek(Date)) %>% 
  group_by(week) %>%
  mutate(WeekEnd = max(Date)) %>% 
  ungroup() %>%
  group_by(WeekEnd, Source) %>%
  summarise(NewCases = sum(NewCases), TotalCases = max(Total)) %>% 
  group_by(Source) %>%
  mutate(TotalCasesPrev = lag(TotalCases, order_by = WeekEnd)) %>%
  mutate(WeeklyMeanRate = NewCases*100/(7*TotalCasesPrev)) %>%
  filter(!is.na(WeeklyMeanRate), WeekEnd <= PrevSunday) %>%
  select(WeekEnd, Source, NewCases, WeeklyMeanRate) %>%
  pivot_longer(-c(WeekEnd, Source), names_to = 'Metric', values_to = 'Value')

p <- ggplot(
  data = dweek,  
  aes(x = WeekEnd, y = Value, group = Source, color = Source, fill=Source)
  ) + facet_wrap(~Metric, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = dmy('24-03-2020'), linetype = '11' ) + 
  theme_minimal() +
  geom_line(linetype = 1, alpha = 0.8, size = 1) + 
  geom_point(alpha = 0.8, size = 3, stroke = 1, shape = 21, fill = 'white') + 
  labs(y = "Number of new cases reported")  + 
  scale_fill_viridis_d(end = 0.6) + scale_color_viridis_d(end=0.6) + 
  coord_cartesian(clip = F) + 
  scale_x_date(breaks = as.Date( unique(dweek$WeekEnd) ), date_labels = "%A, %b %d") + 
  theme(
    axis.text.x = element_text(face = 'bold'),
    panel.grid.minor = element_blank(),
    legend.position = c(0.4, 0.8),
    axis.title.y = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, size = 10, face = 2)
  )

print(p)