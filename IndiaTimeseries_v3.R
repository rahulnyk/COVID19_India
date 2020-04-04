library(tidyverse)
library(lubridate)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
library(janitor)
source('./source_crowd_data.r')


rebuild_dataframe <- F

if (rebuild_dataframe) {
  dc <- build_crowd_data()
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
    "text", x = dmy('25-03-2020'), y = 34, 
    size = 5, label = "Lockdown", hjust = 0, fontface =2 )

p <- p +
  theme_minimal() + 
  theme(
    legend.position = 'off',
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size=10,face="bold")
  ) + 
  scale_color_viridis_d(option="inferno", end = 0.9) + 
  scale_fill_viridis_d(option="inferno", end = 0.9) + 
  scale_y_discrete(position = "right") +
  scale_radius(
    range = c(1, 10),
    trans = "identity",
    guide = "legend"
  ) + 
  coord_cartesian(clip = 'off') 


print(p)



