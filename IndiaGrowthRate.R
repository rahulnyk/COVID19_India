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


rebuild_dataframe <- T

if (rebuild_dataframe) {
  d <- read_csv("https://raw.githubusercontent.com/rahulnyk/covid19_india_data/master/covid_19_india.csv")
}

data_total <- d %>% 
  mutate(ConfirmedForeignNational = ifelse(is.na(ConfirmedForeignNational), 0, ConfirmedForeignNational)) %>%
  rename(state_ut = "State/UnionTerritory", date = "Date") %>%
  mutate(date = dmy(date)) %>%
  group_by(state_ut, date) %>% 
  ## Chosing the max number in case there are multiple entries for same date. 
  summarize(
    ConfirmedIndianNational = max(ConfirmedIndianNational),
    ConfirmedForeignNational = max(ConfirmedForeignNational),
    Recovered = max(Cured), Deceased = max(Deaths)
    ) %>% 
  mutate(Total = ConfirmedIndianNational + ConfirmedForeignNational) %>% 
  group_by(date) %>% summarise(
    Total = sum(Total),
    Recovered = sum(Recovered),
    Deceased = sum(Deceased)
    ) %>%
  mutate(Hospitalized = Total - Recovered - Deceased) %>%
  mutate( Total_next = lead(Total, default = 0, order_by = date), Total_prev = lag(Total, default = 0, order_by = date)) %>%
  mutate( date_next = lead(date, order_by = date), date_prev = lag(date, order_by = date) ) %>% 
  mutate(date_current_prev = as.integer(date - date_prev) , date_next_prev = as.integer(date_next - date_prev) ) %>% 
  mutate( rate1 = round((Total - Total_prev)*100/(date_current_prev*Total_prev)), rate2 = round((Total_next - Total_prev)*100/((date_next_prev)*Total) ) ) %>%
  filter( date > ymd('2020-03-03'))

data_abs <- data_total %>% select(date, Recovered, Hospitalized, Deceased) %>% 
  pivot_longer(-c(date), values_to = "Total", names_to = "type")

data_label <- data_total %>% select(date, Total)
  
plot_theme <- theme_minimal()

pal <- "magma"
p1 <- ggplot(data = data_total,  aes(x = date, y = rate1)) + plot_theme +
  geom_smooth(alpha = 0.2) + 
  geom_point(alpha = 0.3, size = 3) + geom_line(linetype = '12', alpha = 0.6) +
  geom_hline(yintercept = mean(data_total$rate2), linetype = '11', alpha = 0.5) +
  labs(y = "Growth rate in Percentage") + ylim(c(-10, 70)) + 
  geom_label(
    x=dmy('04-03-2020'), 
    y=35, alpha=0.5, hjust=0,
    label= paste("Current Rate = ", round( tail(data_total$rate1, n=1) ), "%", set="") 
    )

p3 <- ggplot(data = data_abs, aes(x = date, y = Total)) + theme_minimal() +
  geom_bar(aes( fill = type ), position = "stack", stat = "identity") + 
  geom_text(data = data_label, aes(label = Total), hjust = -0.2, vjust = 0, size=4, angle=90) +
  ylim(c(0, (max(data_abs$Total)+1000) )) +
  # scale_y_continuous(trans = 'log2') + 
  labs(y = "Cumulative number of cases") + 
  # labs(title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0.2, end = 0.9) +
  scale_color_viridis_d(option=pal, begin = 0.2, end = 0.9) +
  theme(legend.position = c(0.2, 0.6), legend.title = element_blank()) + 
  ylim(c(0, max(data_label$Total)+500 ))

p <- ggarrange(p3, p1, ncol = 1, nrow = 2)

ggsave("gr_output.jpeg", p, device = "jpeg", dpi = 150, width = 4, height = 5)
print(p)






