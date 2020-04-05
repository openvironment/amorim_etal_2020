# GAM bootstraping

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mgcv)
library(caret)
library(recipes)


# Data --------------------------------------------------------------------

tab_model <- read_rds("data/data_model.rds")
tab_bs <- read_rds("data/data_gas_200_bs.rds")

# utils -------------------------------------------------------------------

get_days_sample <- function(df) {
  
  days <-
    df %>%
    distinct(date)
  
  m <- nrow(days)
  
  days %>% 
    sample_n(size = m, replace = TRUE)
  
}

bs_df_gam_plot <- function(fit, i) {
  
  smooth <- fit$smooth[[1]]
  
  x <- fit$model[,smooth$term]
  
  if (smooth$by == "NA") {
    by.level = "NA"
  } else {
    by.level = smooth$by.level
  }
  
  range = tibble(x = x, by = by.level)
  names(range) = c(smooth$term, smooth$by)
  
  par <- smooth$first.para:smooth$last.para
  mat <- PredictMat(smooth, range)
  y <- (mat %*% fit$coefficients[par]) %>% as.numeric
  
  se <- ((mat %*% fit$Vp[par, par, drop = FALSE]) * mat) %>%
    rowSums %>% 
    sqrt
  
  tibble(
    x.val = x,
    value = y,
    int = i
  )
}

gam_fit_func <- function(data, i, formula) {
  
  model <- train(
    form = formula,
    data = na.omit(data),
    method = "gam",
    tuneGrid = data.frame(select = TRUE, method = "GCV.Cp"),
    trControl = trainControl(method = "none")
  )
  
  bs_df_gam_plot(model$finalModel, i)
  
}

bootstrapping <- function(i, df, df_share_bs, ...) {
  
  df_share_bs <- select(df_share_bs, share_bs = i, date)
  
  days <- get_days_sample(df)
  
  df <- df %>% 
    left_join(df_share_bs, by = "date") %>% 
    mutate(share_gas = share_bs) %>% 
    left_join(x = days, y = ., by = "date")
  
  tab <- gam_fit_func(data = df, i, ...)
  
  usethis::ui_done(paste("Resample", i, "completed!"))
  
  tab
  
}

# Data prep ---------------------------------------------------------------

tab_model <- 
  recipe(tab_model) %>% 
  step_dummy(stationname) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  prep(training = tab_model) %>% 
  bake(new_data = tab_model)

formula <- tab_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

# Bootstraping ------------------------------------------------------------

set.seed(5893524) # My student ID

df_gam_plot <- map_dfr(
  1:200,
  bootstrapping,
  df = tab_model,
  df_share_bs = tab_bs,
  formula = formula
)

# Bootstrapping plot

p_boot_gam <- ggplot(df_gam_plot, aes(x = x.val, y = value)) +
  geom_line(aes(group = int), color = 'grey', alpha = 0.3) +
  geom_smooth() +
  labs(
    x = "ShareE25",
    y = "Effect on the ozone concentration"
  ) +
  theme_bw()


