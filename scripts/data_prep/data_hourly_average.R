tab_salvo <- readr::read_rds("data/data_salvo_etal_2017.rds") %>% 
  dplyr::filter(siteid %in% c(1, 2, 3, 5, 6, 7, 15, 18, 22, 27, 29, 31)) %>% 
  dplyr::mutate(
    stationname = dplyr::case_when(
      siteid == 1 ~ "Dom Pedro II",
      siteid == 2 ~ "Santana",
      siteid == 3 ~ "Mooca",
      siteid == 5 ~ "Ibirapuera",
      siteid == 6 ~ "Nossa Senhora do O",
      siteid == 7 ~ "Sao Caetano do Sul",
      siteid == 8 ~ "Congonhas",
      siteid == 10 ~ "Cerqueira Cesa",
      siteid == 15 ~ "Diadema",
      siteid == 18 ~ "Santo Andre 1",
      siteid == 22 ~ "Maua",
      siteid == 27 ~ "Pinheiros",
      siteid == 29 ~ "Parelheiros",
      siteid == 31 ~ "IPEN"
    )
  )

tab_salvo %>%
  dplyr::group_by(hour, stationname) %>%
  dplyr::summarise(o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)) %>% 
  readr::write_rds("data/data_hourly_average.rds")
