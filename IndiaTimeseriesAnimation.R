library(tidyverse)
library(lubridate)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
library(janitor)
source('./source_crowd_data.r')
source('./source_official_data.r')
options(
  gganimate.nframes = 100, 
  gganimate.fps=10
)

### From the notebook
if (F)  {
    dc <- build_crowd_data()
    dc_i <- dc$dc_i
    do_i <- build_official_data()
    d_i_wide <- rbind(do_i, dc_i)
  }
# d_i_long <- d_i_wide %>% pivot_longer(
#   -c(StateUt, Date, Source), 
#   values_to = 'Cumulative', 
#   names_to = 'Status')
## Statewise Data

data_panindia_total<- d_i_wide %>% 
  group_by(Date, Source) %>% 
  summarize(Total = sum(Total)) %>%
  mutate(StateUt = "India Total") %>% 
  ungroup()

data_states_total <- d_i_wide %>% 
  select(StateUt, Source, Date, Total) %>% 
  ungroup()

data_i_total <- rbind(data_panindia_total, data_states_total)

###

### Adding labels to dataframe
data_anim <- data_i_total %>% 
  filter(Source == 'Crowd Source') %>%
  mutate(Day = as.integer(strftime(Date, format = "%j")) ) %>%
  group_by(StateUt) %>% 
  mutate( DaysSince0 = Day - min(Day)  ) %>% 
  mutate( MaxDays = max(DaysSince0) ) %>%
  mutate( MaxCases = max(round(Total))) %>%
  ungroup() %>%
  mutate(label =  paste(Total, StateUt, sep=" | "))
# 
# y_max <- max(data_anim$Total)
# x_max <- max(data_anim$DaysSince0)
# x_label <- x_max + 5
# 


# labels <- data_anim %>%
#   filter(Date == max(Date)) %>%
#   arrange( Total ) %>%
#   mutate(yend = (y_max/n())*row_number()) %>%
#   # mutate( yend = ( 2^(  (log2(y_max)/(n()) )*row_number()  )) ) %>%
#   select(StateUt, yend)

# data_anim2 <- left_join(data_anim, labels, by = c("StateUt") )

p <- ggplot(
      data = data_anim, 
      aes(
          x=reorder(StateUt, -MaxCases), 
          y=DaysSince0, 
          fill = Total^(0.3), 
          size = Total^(0.3), 
          color = Total^(0.3) 
          )
      )  +
  geom_line(alpha=0.3) +
  geom_label(aes(label = as.character(Total) ), size=3, color='white', nudge_x = 0) +
  theme_minimal() +
  theme( 
    legend.position = 'off', 
    axis.text.x = element_text(angle = -45, hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5,  face = "bold", size=12)
    ) +
  coord_cartesian(clip = 'off')  +
  scale_color_viridis_c(option="inferno", end = 1, direction = -1) + 
  scale_fill_viridis_c(option="inferno", end = 1, direction = -1) + 
  ylab("Days since first case reported") # + 
  # scale_radius(
  #   range = c(1, 10),
  #   trans = "identity",
  #   guide = "legend"
  # )

p <- p + labs( title = 'Date {frame_along}', fontface = "bold" ) + 
  transition_time(Date) + ease_aes('cubic-in-out') + 
  transition_reveal(Date)

animate(
  p,
  renderer=gifski_renderer( loop = T ),
  res=150,
  height = 1080,
  width = 1920,
  end_pause = 30
)

anim_save(paste("output", ".gif", sep="" ), animation = last_animation())

# print(p)
