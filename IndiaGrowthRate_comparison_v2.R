library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(readr)
library(ggpubr)
library(janitor)
source('./source_crowd_data.r')
source('./source_official_data.r')

pal <- "cividis"
rebuild_crowdsource_dataframe <- T
rebuild_official_dataframe <- T

if (rebuild_crowdsource_dataframe) {
  dc <- build_crowd_data()
}

if (rebuild_official_dataframe) {
  do <- build_official_data()
}

data <- rbind(do, dc) %>% select(StateUt, Date, Total, Source) %>% 
  group_by(Date, Source) %>%
  summarise(Total = sum(Total)) %>% group_by(Source) %>%
  mutate( TotalPrev = lag(Total, default = 0, order_by = Date)) %>%
  mutate(  DatePrev = lag(Date, order_by = Date) ) %>% 
  mutate(DateCurPrev = as.integer(Date - DatePrev) ) %>%
  filter( Date > ymd('2020-03-04')	) %>%
  mutate( DailyRate = round((Total - TotalPrev)*100/(DateCurPrev*TotalPrev) ) ) %>%
  ungroup()
  

plot_theme <-   theme(
  axis.text=element_text(size=8, color = "darkgrey"), 
  axis.title=element_text(size=10),
  plot.title = element_text(size = 12, hjust = 0),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  legend.position = c(0.8, 0.8),
  panel.background = element_rect(fill = "white")
) 

p1 <- ggplot(data = data,  aes(x = Date, y = DailyRate, group = Source, color = Source, fill=Source)) + plot_theme +
  geom_point(alpha = 0.3, size = 2) + geom_line(linetype = 1, alpha = 0.2) + 
  geom_smooth(alpha = 0.2, size=0.6) +
  labs(y = "Growth rate in Percentage")  + 
  ylim(c(-10, 50)) +
  geom_label( color = "white", fill="black",
              x=dmy('06-03-2020'), 
              y=40, alpha=0.5, hjust=0,
              label= paste("Yesterdays Rate = ", round( tail(data$DailyRate, n=2)[1] ), "%", sep = "") 
  ) +
  scale_fill_viridis_d(end = 0.6) + scale_color_viridis_d(end=0.6)

print(p1)
