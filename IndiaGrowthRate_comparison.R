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
library(janitor)

options(
  gganimate.nframes = 40, 
  gganimate.fps=10
)

pal <- "magma"
pal2 <- "cividis"
rebuild_dataframe <- T
yesterday <- Sys.Date() -1
today <- Sys.Date()

if (rebuild_dataframe) {
  # d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pub?output=csv'
  
  d_official <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/IndiaOfficalData.csv")
  
  d <- read_csv(url) %>% clean_names() %>%
    select(-c("source_1", "source_2", "source_3", "backup_notes", "notes", "estimated_onset_date")) %>%
    filter(! is.na(date_announced) ) %>%
    mutate(date = dmy(date_announced)) %>% 
    select(date, detected_state, current_status) %>% 
    rename(state_ut = "detected_state", status = "current_status" ) %>%
    arrange(date)
    
  dt <- NULL
  
  for (di in as.list( unique(d$date) )) {
    print(date)
    dts <- d %>%
      filter(date <= di) %>% group_by(state_ut, status) %>%
      summarize(total = n() ) %>% mutate(date = di)
    dt <- rbind(dt, dts)
  }
  
}

data_total <- dt %>% group_by(date, status) %>% summarize(total = sum(total)) %>%
  pivot_wider(names_from = status, values_from = total, values_fill = list(total = 0)) %>%
  mutate(total = Hospitalized + Recovered + Deceased + Migrated) %>% ungroup() %>%
  mutate( total_next = lead(total, default = 0, order_by = date), total_prev = lag(total, default = 0, order_by = date)) %>%
  mutate( date_next = lead(date, order_by = date), date_prev = lag(date, order_by = date) ) %>% 
  mutate(date_current_prev = as.integer(date - date_prev) , date_next_prev = as.integer(date_next - date_prev) ) %>%
  filter(date > ymd('2020-03-03')	) %>%
  mutate( rate1 = round((total - total_prev)*100/(date_current_prev*total_prev) ), rate2 = round((total_next - total_prev)*100/((date_next_prev)*total)) ) %>%
  ungroup()

data_total_official <- d_official %>% mutate(date = mdy(date)) %>% rename(total = "Total") %>%
  mutate( total_next = lead(total, default = 0, order_by = date), total_prev = lag(total, default = 0, order_by = date)) %>%
  mutate( date_next = lead(date, order_by = date), date_prev = lag(date, order_by = date) ) %>% 
  mutate(date_current_prev = difftime(date, date_prev, units = "days") , date_next_prev = difftime(date_next, date_prev, units = "days") ) %>%
  mutate( rate1 = round((total - total_prev)*100/(as.integer(date_current_prev)*total_prev) ), rate2 = round((total_next - total_prev)*100/(as.integer(date_next_prev)*total)) ) %>%
  ungroup() %>% filter(date > ymd('2020-03-03')	) 

data_rate <- data_total %>% select(date, rate1, rate2) %>% mutate(source = "Crowd")

data_rate_official <- data_total_official %>% select(date, rate1, rate2) %>% mutate(source = "Official")

data_rate <- rbind(data_rate, data_rate_official)

data_abs <- data_total %>% mutate(source = "Crowd Sourced Data", total = total)  %>% select(date, source, total) 
data_abs_official <- data_total_official %>% mutate(source = "Official Data", total = total) %>% select(date, source, total) 

data_abs <- rbind(data_abs, data_abs_official) %>% group_by(source) %>%
  mutate(max_cases = max(total)) %>% ungroup() %>%
  mutate(label = paste(source, max_cases, sep=" | ") )


data_label <- data_total %>% select(date, total) %>% rename(total = "total")
  
plot_theme <-   theme(
    axis.text=element_text(size=8, color = "darkgrey"), 
    axis.title=element_text(size=10),
    plot.title = element_text(size = 12, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = 'off',
    panel.background = element_rect(fill = "white")
  ) 

p1 <- ggplot(data = data_rate,  aes(x = date, y = rate1, group = source, color = source, fill=source)) + plot_theme +
  geom_smooth(alpha = 0.2) +
  geom_point(alpha = 0.4, size = 2) + geom_line(linetype = '12', alpha = 0.6) 

p1 <- p1 + labs(y = "Growth rate in Percentage")  + 
  ylim(c(-10, 50)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_label( color = "darkgrey", fill="white",
    x=dmy('04-03-2020'), 
    y=40, alpha=0.5, hjust=0,
    label= paste("Yesterdays Rate = ", round( tail(data_total$rate1, n=2)[1] ), "%", set="") 
    ) +
  scale_fill_viridis_d(option=pal, begin = 0.1, end = 0.8) + 
  scale_color_viridis_d(option=pal, begin = 0.1, end = 0.8) 

p3 <- ggplot(data = data_abs, aes(x = date, y = total)) + plot_theme +
  geom_bar(aes( fill = label), position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity") + 
  # geom_text(data = data_label, aes(label = total), hjust = -0.2, vjust = 0, size=4, angle=90) +
  # scale_y_continuous(trans = 'log2') + 
  labs(y = "Cumulative number of cases") + 
  scale_fill_viridis_d(option=pal, begin = 0.1, end = 0.8) + 
  scale_color_viridis_d(option=pal, begin = 0.1, end = 0.8) +
  theme(legend.position = c(0.4, 0.7), legend.title = element_blank() ) 

p <- ggarrange(
  p1, p3,
  ncol = 1, 
  nrow = 2
  )

ggsave("gr_output.jpeg", p, device = "jpeg", dpi = 150, width = 4, height = 5)
print(p)


