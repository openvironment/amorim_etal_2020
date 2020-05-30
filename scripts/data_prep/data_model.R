# Transform Salvo et al, 2017 original dataset into data_model.rds

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Salvo et al, 2017 dataset -----------------------------------------------

tab_salvo <- read_rds("data/data_salvo_etal_2017.rds")

# Transformations ---------------------------------------------------------

o3_stations <- c(1, 2, 3, 5, 6, 7, 15, 18, 22, 27, 29, 31)

tab <- tab_salvo %>% 
  select(
    date, year, month, week, day, dayofweek, hour, 
    dv_publicholiday, dv_weekday_regular, dv_yearendvacation,
    siteid, 
    o3_mass_conc, share_gas, share_gas_aggr,
    rd, tp, hm, ws, pp, ends_with("9am"),
    starts_with("congestion"), dv_beltway_open
  ) %>% 
  mutate(
    dv_o3 = ifelse(siteid %in% o3_stations, 1, 0),
    date = ymd(str_c(year, month, day, sep = "-")),
    stationname = case_when(
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

morning_vars <- tab %>%
  filter(hour %in% 7:11) %>% 
  select(date, stationname, starts_with("congestion")) %>% 
  group_by(date, stationname) %>% 
  summarise_all(~mean(.x, na.rm = TRUE)) %>% 
  ungroup()

afternoon_vars <- tab %>% 
  filter(hour %in% 12:16) %>% 
  select(date, stationname, o3_mass_conc, share_gas, rd:ws, pp) %>% 
  group_by(date, stationname) %>% 
  summarise_all(~mean(.x, na.rm = TRUE)) %>% 
  ungroup()

other_vars <- tab %>% 
  select(
    date, 
    stationname,
    siteid,
    year:dayofweek, 
    starts_with("dv_")
  ) %>% 
  group_by(date, stationname) %>% 
  summarise_all(~first(.x)) %>% 
  ungroup()

tab_model <- morning_vars %>% 
  inner_join(afternoon_vars, by = c("date", "stationname")) %>% 
  inner_join(other_vars, by = c("date", "stationname")) %>% 
  filter(dv_o3 == 1) %>% 
  select(-dv_o3)

# Validation
# 
# df <- tab_model %>% 
#   filter(!month %in% 6:9) %>% 
#   na.omit()
# 
# nrow(df) == 13203 
# round(mean(df$o3_mass_conc), 1) == 72.2 

# tab_model <- tab_model %>%
#   group_by(stationname) %>%
#   mutate(
#     trend = date - lubridate::ymd("2008-11-01"),
#     trend = as.numeric(trend) / 365.25
#   ) %>%
#   ungroup() %>%
#   filter(!month %in% 6:9)

# Saving tab_model --------------------------------------------------------

write_rds(
  tab_model,
  path = "data/data_model.rds", 
  compress = "gz" 
)

