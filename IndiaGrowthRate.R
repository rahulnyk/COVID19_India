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
  rename(state_ut = "State/UnionTerritory", sn = "Sno", Date = "Date") %>%
  mutate(date = dmy(Date)) %>%
  group_by(state_ut, date) %>% 
  ## Chosing the max number in case there are multiple entries for same date. 
  summarize(
    ConfirmedIndianNational = max(ConfirmedIndianNational),
    ConfirmedForeignNational = max(ConfirmedForeignNational),
    cured = max(Cured), deaths = max(Deaths)
    ) %>% 
  mutate(total = ConfirmedIndianNational + ConfirmedForeignNational) %>% 
  group_by(date) %>% summarise(total = sum(total), cured = sum(cured), deaths = sum(deaths)) %>%
  mutate(in_treatment = total - cured - deaths) %>%
  mutate( total_next = lead(total, default = 0), total_prev = lag(total, default = 0)) %>%
  mutate( rate1 = round((total - total_prev)*100/total), rate2 = round((total_next - total_prev)*100/(2*total)) ) %>%
  filter( date > '2020-03-03') %>% select(-c("total")) 

data_rate <- data_total %>% select(date, rate1, rate2)

data_abs <- data_total %>% select(date, cured, in_treatment, deaths) %>% 
  pivot_longer(-c(date), values_to = "total", names_to = "type")

plot_theme <-   theme(
    axis.text=element_text(size=8, color = "darkgrey"), 
    axis.title=element_text(size=10),
    plot.title = element_text(size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = 'off',
    panel.background = element_rect(fill = "white")
  ) 

p1 <- ggplot(data = data_rate,  aes(x = date, y = rate2)) + plot_theme +
  geom_smooth(alpha = 0.2) + 
  geom_point(alpha = 0.3) + geom_line(linetype = '12', alpha = 0.6) +
  geom_hline(yintercept = mean(data_total$rate2), linetype = '11', alpha = 0.5) +
  labs(y = "Growth rate in Percentage", title = "Groth rate by date") + 
  ylim(c(-10, 100)) + 
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.9)

p3 <- ggplot(data = data_abs, aes(x = date, y = total)) + plot_theme +
  geom_bar(aes( fill = type ), position = "stack", stat = "identity") + 
  # scale_y_continuous(trans = 'log2') + 
  labs(y = "Cumulative, Log", title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0.2, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0.2, end = 0.9) +
  theme(legend.position = c(0.2, 0.6), legend.title = element_blank()) 

p <- ggarrange(
  p1, p3,
  ncol = 1, 
  nrow = 2
  )

ggsave("rg_output.jpeg", p, device = "jpeg", dpi = 150, width = 4, height = 5)
print(p)






