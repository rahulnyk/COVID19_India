library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
library(janitor)
source('./source_crowd_data.r')
source('./source_official_data.r')

pal <- "cividis"
rebuild_crowdsource_dataframe <- F
rebuild_official_dataframe <- F

if (rebuild_crowdsource_dataframe) {
  dc <- build_crowd_data()
}

if (rebuild_official_dataframe) {
  do <- build_official_data()
}

data <- rbind(do, dc) %>% 
  pivot_longer(-c(StateUt, Date, Source), values_to = "Cumulative", names_to = "Status") %>%
  group_by(Date, Source, Status) %>% 
  summarise(Cumulative = sum(Cumulative)) %>% 
  filter( Date > dmy('03-03-2020')) %>%
  ungroup() 
  
data_abs <- data %>% filter(Status != 'Total')

data_label <- data %>% filter(Status == 'Total') %>% 
  filter( Date > Sys.Date()-5 ) # %>%
  # filter( Date < Sys.Date() )

data_today <- data_abs %>% 
  filter(Date == Sys.Date()) %>% 
  pivot_wider(values_from = Cumulative, names_from = Source)

plot_theme <-  theme_minimal() 
# + theme(
#   axis.text=element_text(size=8, color = "darkgrey"),
#   axis.title=element_text(size=10),
#   plot.title = element_text(size = 12, hjust = 0),
#   legend.position = c(0.2, 0.8)
# ) 

p1 <- ggplot(data = data_abs,  aes(x = Date, y = Cumulative, fill=Status)) + theme_minimal() +
  geom_area(stat = 'identity') + facet_wrap(~Source, ncol = 1) + 
  geom_text(
    data = data_label, 
    aes(label = Cumulative), show.legend = F,
    angle = 0, color='darkgrey', vjust = 0.5, hjust = 1, nudge_y = 0, nudge_x = -0.5) + 
  theme(legend.position = "top") +
  scale_fill_viridis_d(begin = 0.1, end = 1) + scale_color_viridis_d(begin = 0.1, end=1)

print(p1)
