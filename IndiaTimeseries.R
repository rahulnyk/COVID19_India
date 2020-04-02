setwd("~/work/covid19/COVID19_IndiaData")
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
library(readr)

options(
  gganimate.nframes = 100, 
  gganimate.fps=10
)

animate <- T
rebuild_dataframe <- T
yesterday <- Sys.Date() 

if (rebuild_dataframe) {
  d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")
}
data_state <- d %>% 
  rename(state_ut = "State/UnionTerritory", sn = "Sno", Date = "Date") %>%
  mutate(date = dmy(Date)) %>%
  mutate(ConfirmedForeignNational = ifelse(is.na(ConfirmedForeignNational), 0, ConfirmedForeignNational)) %>%
  ###
  group_by(state_ut, date) %>% 
  ## Chosing the max number in case there are multiple entries for same date. 
  summarize(
    ConfirmedIndianNational = max(ConfirmedIndianNational),
    ConfirmedForeignNational = max(ConfirmedForeignNational),
    Cured = max(Cured),
    Deaths = max(Deaths)
    ) %>% 
  mutate(cumulative = ConfirmedIndianNational + ConfirmedForeignNational) %>% 
  filter(cumulative != 0) %>% 
  select(state_ut, date, cumulative) %>% ungroup()

data_total <- data_state %>% group_by(date) %>% summarize(cumulative = sum(cumulative)) %>%
  mutate(state_ut = "India Total") %>% ungroup()

data <- rbind(data_state, data_total) %>% 
  mutate(day = as.integer(strftime(date, format = "%j")) ) %>%
  ###
  group_by(state_ut) %>% 
  mutate( days_since_0 = day - min(day)  ) %>% 
  mutate( max_days = max(days_since_0) ) %>%
  ungroup() %>%
  ###
  filter(cumulative > 2) %>%
  mutate(label =  paste(cumulative, state_ut, sep=" | "))

y_max <- max(data$cumulative)
x_max <- max(data$days_since_0)
x_label <- x_max + 5

labels <- data %>% 
  filter(date == yesterday) %>% 
  arrange( cumulative ) %>% 
  # mutate(yend = (y_max/n())*row_number()) %>%
  mutate( yend = ( 2^(  (log2(y_max)/(n()) )*row_number()  )) ) %>%
  select(state_ut, yend)

data <- left_join(data, labels, by = c("state_ut") )


if (!animate) {
  data <- data %>% filter(date == yesterday)
}

p <- ggplot(data = data, aes(x=days_since_0, y=cumulative, color = state_ut, size = cumulative) ) +
  geom_point() + 
  geom_line(size = 0.5, alpha=0.6)  +
  geom_label(
    aes(x=x_label, y = yend, label = label, fill=state_ut),
    color='white', fontface = "bold",
    hjust = 0, size=3
  ) + 
  geom_segment(aes(xend = x_label, yend = yend), linetype = "11", size=0.3, alpha=0.3)

p <- p +
  theme_minimal() +
  theme(
    axis.text=element_text(size=10, color = "darkgrey"),
    axis.title=element_text(size=12)
  ) +
  theme( legend.position = 'off' ) +
  theme(plot.title = element_text(size = 16, hjust = 0.5) ) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(plot.margin = margin(15, 160, 15, 10)) +
  coord_cartesian(clip = 'off')  +
  scale_color_viridis_d(option="inferno", begin = 0, end = 0.9) + 
  scale_fill_viridis_d(option="inferno", begin = 0, end = 0.9) + 
  xlab("Number of day since first report") + 
  ylab("Number of cases (Cumulative)")  + 
  scale_y_continuous(trans = 'log2')

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
  print(p)
  ggsave("output.jpg", 
         width = 6.4,
         height = 3.6
         )
}



