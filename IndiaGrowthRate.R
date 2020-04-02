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
animate <- F
rebuild_dataframe <- F
yesterday <- Sys.Date() -1
today <- Sys.Date()

if (rebuild_dataframe) {
  # d <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv")
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pub?output=csv'
  
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
  mutate(Total = Hospitalized + Recovered + Deceased + Migrated) %>% 
  ungroup() %>% arrange(date) %>%
  mutate( total_next = lead(Total, default = 0), total_prev = lag(Total, default = 0)) %>%
  mutate( rate1 = round((Total - total_prev)*100/total_prev), rate2 = round((total_next - total_prev)*100/(2*Total)) ) %>%
  filter( date > dmy('03-03-2020')) %>% ungroup()

data_rate <- data_total %>% select(date, rate1, rate2) 

data_abs <- data_total %>% select(date, Recovered, Hospitalized, Deceased) %>% 
  pivot_longer(-c(date), values_to = "total", names_to = "status") %>% 
  group_by(status) %>% mutate(max_cases = max(total)) %>% ungroup() %>%
  mutate(label = paste(status, max_cases, sep=" | ") )

data_label <- data_total %>% select(date, Total) %>% rename(total = "Total")
  
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
  ylim(c(-20, 100)) + 
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.9) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_label(
    x=dmy('04-03-2020'), 
    y=50, alpha=0.5, hjust=0,
    label= paste("Current Rate = ", round( tail(data_total$rate1, n=1) ), "%", set="") 
    )

p3 <- ggplot(data = data_abs, aes(x = date, y = total)) + plot_theme +
  geom_bar(aes( fill = label ), position = "stack", stat = "identity") + 
  geom_text(data = data_label, aes(label = total), hjust = -0.2, vjust = 0, size=4, angle=90) +
  # scale_y_continuous(trans = 'log2') + 
  labs(y = "Cumulative number of cases") + # labs(title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0.2, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0.2, end = 0.9) +
  theme(legend.position = c(0.2, 0.6), legend.title = element_blank()) + ylim(c(0, max(data_label$total)+200 ))

p <- ggarrange(
  p1, p3,
  ncol = 1, 
  nrow = 2
  )

ggsave("gr_output.jpeg", p, device = "jpeg", dpi = 150, width = 4, height = 5)
print(p)


