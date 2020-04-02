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
library(ggpubr)

options(
  gganimate.nframes = 40, 
  gganimate.fps=10
)

pal <- "magma"
animate <- F
rebuild_dataframe <- T
yesterday <- Sys.Date()

if (rebuild_dataframe) {
  d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")

}
data_total <- d %>% 
  mutate(ConfirmedForeignNational = ifelse(is.na(ConfirmedForeignNational), 0, ConfirmedForeignNational)) %>%
  rename(state_ut = "State/UnionTerritory", sn = "Sno", Date = "Date") %>%
  mutate(date = dmy(Date)) %>%
  group_by(state_ut, date) %>% 
  ## Chosing the max number in case there are multiple entries for same date. 
  summarize(
    ConfirmedIndianNational = max(ConfirmedIndianNational),
    ConfirmedForeignNational = max(ConfirmedForeignNational),
    Recovered = max(Cured), Deceased = max(Deaths)
    ) %>% 
  mutate(total = ConfirmedIndianNational + ConfirmedForeignNational) %>% 
  group_by(date) %>% summarise(
    total = sum(total),
    Recovered = sum(Recovered),
    Deceased = sum(Deceased)
    ) %>%
  mutate(Hospitalized = total - Recovered - Deceased) %>%
  mutate( total_next = lead(total, default = 0, order_by = date), total_prev = lag(total, default = 0, order_by = date)) %>%
  mutate( date_next = lead(date, order_by = date), date_prev = lag(date, order_by = date) ) %>% 
  mutate(date_current_prev = as.integer(date - date_prev) , date_next_prev = as.integer(date_next - date_prev) ) %>% 
  mutate( rate1 = round((total - total_prev)*100/(date_current_prev*total_prev)), rate2 = round((total_next - total_prev)*100/((date_next_prev)*total) ) ) %>%
  filter( date > ymd('2020-03-03')) # %>% filter(date < Sys.Date())

data_rate <- data_total %>% select(date, rate1, rate2)

data_abs <- data_total %>% select(date, Recovered, Hospitalized, Deceased) %>% 
  pivot_longer(-c(date), values_to = "total", names_to = "type")

data_label <- data_total %>% select(date, total)
  
plot_theme <-   theme(
    axis.text=element_text(size=8, color = "darkgrey"), 
    axis.title=element_text(size=10),
    plot.title = element_text(size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = 'off',
    panel.background = element_rect(fill = "white")
  ) 

p1 <- ggplot(data = data_rate,  aes(x = date, y = rate1)) + plot_theme +
  geom_smooth(alpha = 0.2) + 
  geom_point(alpha = 0.3, size = 3) + geom_line(linetype = '12', alpha = 0.6) +
  geom_hline(yintercept = mean(data_total$rate2), linetype = '11', alpha = 0.5) +
  labs(y = "Growth rate in Percentage") + # labs(title = "Groth rate by date") + 
  ylim(c(-10, 70)) + 
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.9) +
  theme() +
  geom_label(
    x=dmy('04-03-2020'), 
    y=35, alpha=0.5, hjust=0,
    label= paste("Current Rate = ", round( tail(data_total$rate1, n=1) ), "%", set="") 
    )

p3 <- ggplot(data = data_abs, aes(x = date, y = total)) + plot_theme +
  geom_bar(aes( fill = type ), position = "stack", stat = "identity") + 
  geom_text(data = data_label, aes(label = total), hjust = -0.2, vjust = 0, size=4, angle=90) +
  # scale_y_continuous(trans = 'log2') + 
  labs(y = "Cumulative number of cases") + # labs(title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0.2, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0.2, end = 0.9) +
  theme(
    legend.position = c(0.2, 0.6), 
    legend.title = element_blank(),
    plot.margin = margin(20, 0, 0, 0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
    ) + ylim(c(0, max(data_label$total)+200 ))

p <- ggarrange(
  p3, p1,
  ncol = 1, 
  nrow = 2
  )

ggsave("gr_output.jpeg", p, device = "jpeg", dpi = 150, width = 4, height = 5)
print(p)






