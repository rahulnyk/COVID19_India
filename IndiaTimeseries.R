setwd("~/work/covid19/COVID19India/COVID19_IndiaData")
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
  gganimate.nframes = 144, 
  gganimate.fps=12
)

animate <- T

yesterday <- Sys.Date()-1

library(readr)
d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")

data <- d %>% 
  rename(state_ut = "State/UnionTerritory", sn = "Sno", Date = "Date") %>%
  mutate(date = dmy(Date)) %>%
  ###
  group_by(state_ut, date) %>% 
  summarize(
    ConfirmedIndianNational = max(ConfirmedIndianNational),
    ConfirmedForeignNational = max(ConfirmedForeignNational),
    Cured = max(Cured),
    Deaths = max(Deaths)
    ) %>% 
  mutate(cumulative = ConfirmedIndianNational + ConfirmedForeignNational) %>% 
  filter(cumulative != 0) %>% 
  select(state_ut, date, cumulative) %>% 
  mutate(day = as.integer(strftime(date, format = "%j")) ) %>%
  ###
  group_by(state_ut) %>% 
  mutate( days_since_0 = day - min(day)  ) %>% 
  mutate( max_days = max(days_since_0) ) %>%
  ungroup() %>%
  ###
  filter(max_days > 5) %>%
  mutate(label =  paste(cumulative, state_ut, sep=" | "))

y_max <- max(data$cumulative)
x_max <- max(data$days_since_0)
x_label <- x_max + 5

labels <- data %>% 
  filter(date == yesterday) %>% 
  arrange( cumulative ) %>% 
  mutate(yend = (y_max/n())*row_number()) %>%
  select(state_ut, yend)

data <- left_join(data, labels, by = c("state_ut") )

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
  ylab("Number of cases (Cumulative)")



if (animate) {
  p <- p + labs(
    title = 'Date {frame_along}', fontface = "bold"
  )
  p <- p + 
    transition_time(date) + ease_aes('cubic-in-out') + 
    transition_reveal(date)
  animate(
    p,
    renderer=gifski_renderer( loop = T ), # render gif
    # renderer=av_renderer(), # render video
    res=150,
    height = 720,
    width = 1280
  )
  anim_save(paste('IndiaCovid19', (Sys.Date()-1), "_1", ".gif", sep="" ), animation = last_animation())
} else {
  print(p)
  ggsave(paste('IndiaCovid19', (Sys.Date()-1), "_1", ".jpg", sep="" ), 
         width = 720,
         height = 1280
         )
}



