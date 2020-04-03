
build_crowd_data <- function () {
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pub?output=csv'
  dc <- read_csv(url) %>% clean_names() %>%
    select(-c("source_1", "source_2", "source_3", "backup_notes", "notes", "estimated_onset_date")) %>%
    filter(! is.na(date_announced) ) %>%
    mutate(date = dmy(date_announced)) %>% 
    select(date, detected_state, current_status) %>% 
    rename(StateUt = "detected_state", Status = "current_status" ) %>%
    arrange(date)
  dtcs <- NULL
  for (di in as.list( unique(dc$date) )) {
    print(di)
    dts <- dc %>%
      filter(date <= di) %>% group_by(StateUt, Status) %>%
      summarize(total = n() ) %>% mutate(Date = di, Source = 'Crowd Source')
    dtcs <- rbind(dtcs, dts)
  }
  dtc <- dtcs %>% 
    pivot_wider(names_from = Status, values_from = total, values_fill = list(total = 0)) %>%
    mutate(Total = Recovered + Hospitalized + Deceased + Migrated) %>% select(-c(Migrated))
  return(dtc)
}