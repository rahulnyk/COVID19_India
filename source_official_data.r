
build_official_data <- function () {
  do <- read_csv("https://raw.githubusercontent.com/rahulnyk/COVID19_IndiaData/master/covid_19_india.csv") %>%
    mutate(ConfirmedForeignNational = ifelse(is.na(ConfirmedForeignNational), 0, ConfirmedForeignNational)) %>%
    rename(StateUt = "State/UnionTerritory") %>%
    mutate(Date = dmy(Date)) %>%
    group_by(StateUt, Date) %>% 
    ## Chosing the max number in case there are multiple entries for same date. 
    summarize(
      Total =  max(ConfirmedIndianNational) + max(ConfirmedForeignNational),
      Recovered = max(Cured), 
      Deceased = max(Deaths),
      Hospitalized = Total-Recovered-Deceased
    ) %>% mutate(Source = "Official")
  return(do)
}