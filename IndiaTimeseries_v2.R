library(tidyverse)
library(lubridate)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
source('./source_crowd_data.r')

options(
  gganimate.nframes = 100, 
  gganimate.fps=10
)

animate <- F
rebuild_dataframe <- F
yesterday <- Sys.Date()-1

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
  ungroup() %>%
  ###
  filter(Total > 2) %>%
  mutate(label =  paste(Total, StateUt, sep=" | "))

y_max <- max(data$Total)
x_max <- max(data$DaysSince0)
x_label <- x_max + 5

labels <- data %>% 
  filter(Date == yesterday) %>% 
  arrange( Total ) %>% 
  # mutate(yend = (y_max/n())*row_number()) %>%
  mutate( yend = ( 2^(  (log2(y_max)/(n()) )*row_number()  )) ) %>%
  select(StateUt, yend)

data <- left_join(data, labels, by = c("StateUt") )


if (!animate) {
  data <- data %>% filter(Date == yesterday)
}

p <- ggplot(data = data, aes(x=DaysSince0, y=Total, fill = StateUt, size = Total^(0.3) ))  +
  geom_point(shape = 21, color = 'lightgrey') + 
  geom_line(size = 0.5, alpha=0.6)  +
  geom_label(
    aes(x=x_label, y = yend, label = label, fill=StateUt),
    color='white', fontface = "bold",
    hjust = 0, size=3) + 
  geom_segment(aes(xend = x_label, yend = yend, color = StateUt), linetype = "11", size=0.4, alpha=0.5)

p <- p +
  theme_minimal() +
  theme(
    axis.text=element_text(size=10, color = "darkgrey"),
    axis.title=element_text(size=12)
  ) +
  theme( legend.position = 'off' ) +
  theme(plot.title = element_text(size = 12, hjust = 0.5) ) +
  theme(plot.margin = margin(15, 160, 15, 10)) +
  coord_cartesian(clip = 'off')  +
  scale_color_viridis_d(option="inferno", end = 0.9) + 
  scale_fill_viridis_d(option="inferno", end = 0.9) + 
  xlab("Number of day since first report") + 
  ylab("Number of cases (Cumulative)")  + 
  scale_y_continuous(trans = 'log2') + scale_radius(
    range = c(1, 15),
    trans = "identity",
    guide = "legend"
  )

if (animate) {
  p <- p + labs(
    title = 'Date {frame_along}', fontface = "bold"
  )
  p <- p + 
    transition_time(date) + ease_aes('cubic-in-out') + 
    transition_reveal(date)
  animate(
    p,
    renderer=gifski_renderer( loop = F ), # render gif
    # renderer=av_renderer(), # render video
    res=150,
    height = 720,
    width = 1280
  )
  anim_save(paste("output", ".gif", sep="" ), animation = last_animation())
} else {
  p <- p + labs(
    title = paste('Total number of cases on ', format(Sys.Date(), "%B %d %Y") ), fontface = "bold"
  )
  print(p)
  ggsave("output.jpg", 
         width = 6.4,
         height = 3.6
         )
}



