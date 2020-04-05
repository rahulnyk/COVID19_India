library(tidyverse)
library(lubridate)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
library(janitor)
source('./source_crowd_data.r')


rebuild_dataframe <- T

if (rebuild_dataframe) {
  dc <- build_crowd_data()
  dc <- dc$dc_i
}

data_total <- dc %>% group_by(Date) %>% summarize(Total = sum(Total)) %>%
  mutate(StateUt = "India Total") %>% ungroup()

data_state <- dc %>% select(StateUt, Date, Total) %>% ungroup()


data <- rbind(data_total, data_state) %>%
  mutate(Day = as.integer(strftime(Date, format = "%j")) ) %>%
  ###
  group_by(StateUt) %>% 
  mutate( DaysSince0 = Day - min(Day)  ) %>% 
  mutate( MaxDays = max(DaysSince0) ) %>%
  mutate( MaxCases = max(Total) ) %>%
  ungroup() %>%
  ###
  mutate(label =  paste(MaxCases, StateUt, sep=" | ")) %>%
  filter(Date > dmy('01-03-2020'))


p <- ggplot(data = data, aes(x=Date, y=reorder(label, MaxCases), fill = StateUt, size = Total^(0.3) ))  +
  geom_point(shape = 21, color = 'lightgrey') + 
  geom_vline(xintercept = dmy('24-03-2020'), linetype = '11' ) + 
  annotate(
    "text", x = dmy('24-03-2020'), y = 32, 
    size = 4, label = "Lockdown", hjust = 0, vjust = -0.5, fontface =1, angle = 30 )

p <- p +
  theme_minimal() + 
  theme(
    legend.position = 'off',
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size=10,face="bold"),
    axis.text.x = element_text(angle = 30, hjust=0)
  ) + 
  scale_color_viridis_d(option="inferno", end = 0.9) + 
  scale_fill_viridis_d(option="inferno", end = 0.9) + 
  scale_y_discrete(position = "right") +
  scale_x_date(position = "top", date_breaks = '7 days') +
  scale_radius(
    range = c(1, 10),
    trans = "identity",
    guide = "legend"
  ) + 
  coord_cartesian(clip = 'off') 


print(p)



