
build_crowd_data <- function () {
  url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pub?output=csv'
  dc <- read_csv(url) %>% clean_names() %>%
    select(-c("source_1", "source_2", "source_3", "backup_notes", "notes", "estimated_onset_date")) %>%
    filter(! is.na(date_announced) ) %>%
    mutate(Date = dmy(date_announced)) %>%
    select(Date, detected_state, detected_city, age_bracket, current_status) %>%
    rename(
      StateUt = "detected_state", 
      City = "detected_city",
      Status = "current_status", 
      AgeBracket = "age_bracket"
      ) %>%
    arrange(Date)
  dtcs <- NULL
  for (di in as.list( unique(dc$Date) )) {
    print(di)
    dts <- dc %>%
      filter(Date <= di) %>% group_by(StateUt, Status) %>%
      summarize(total = n() ) %>% mutate(Date = di, Source = 'Crowd Source')
    dtcs <- rbind(dtcs, dts)
  }
  dtc <- dtcs %>% 
    pivot_wider(names_from = Status, values_from = total, values_fill = list(total = 0)) %>%
    mutate(Total = Recovered + Hospitalized + Deceased + Migrated) %>% select(-c(Migrated))
  cs_data_list <- list(dc_i = dtc, dc_raw_i = dc)
  return(cs_data_list)
}