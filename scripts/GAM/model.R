# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mgcv)
library(caret)
library(recipes)

source("scripts/utils/utils_modelling.R")

# Data --------------------------------------------------------------------

tab_model <- read_rds("data/data_model.rds")

tab_model <- tab_model %>% 
  filter(
    stationname == "Dom Pedro II"
  ) %>% 
  select(
    -siteid, -stationname,
    -date, -year, -week, -day, -dv_weekday_regular, -dv_yearendvacation,
    -dv_beltway_open
  ) %>% 
  na.omit()

# tab_bs <- read_rds("data/data_gas_200_bs.rds")

# Data prep ---------------------------------------------------------------

rec <- tab_model %>% 
  slice(1) %>% 
  recipe(o3_mass_conc ~ .) %>% 
  step_interact(terms = ~ dayofweek:dv_publicholiday) %>%
  step_mutate(
    month = as.factor(month), 
    dayofweek = as.factor(dayofweek),
    dayofweek_x_dv_publicholiday = as.factor(dayofweek_x_dv_publicholiday)
  ) %>% 
  step_dummy(month, starts_with("dayofweek"))

# rec %>% prep(tab_model) %>% bake(tab_model)

# Fit Gaussian ------------------------------------------------------------

v <- 10
r <- 5

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:(v * r), ~sample.int(1000, 2))
seeds[[v * r + 1]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "repeatedcv", 
  number = v, 
  repeats = r,
  verboseIter = TRUE,
  seeds = seeds
)

link = "identity"

model <- train(
  x = rec,
  data = tab_model,
  method = "gam",
  family = gaussian(link = link),
  trControl = train_control
)

# model
# summary(model$finalModel)  
# varImp(model)

# Metrics
tab_metrics <- model$results %>% 
  filter(select == model$bestTune$select) %>% 
  extract_gam_metrics(v * r) %>% 
  mutate(link_function = link)

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/gam_gaussian_metrics.rds"
)

# Estimates
tab_estimates <- model$finalModel %>% 
  summary() %>% 
  extract_gam_estimates()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/gam_gaussian_estimates.rds"
)

# Smooth term plot

p_gam <- gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "ShareE25",
  ylab = "Effect on the ozone concentration"
)

ggsave(
  "results/dom_pedro_ii/gam_gaussian_plot.png",
  plot = p_gam,
  width = 16,
  height = 10
)

# Observed vs predicted plot

p_pred <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("Gaussian")

ggsave(
  "results/dom_pedro_ii/gam_gaussian_pred_plot.png",
  plot = p_pred,
  width = 16,
  height = 10
)




# Fit Gamma ---------------------------------------------------------------

tab_model_gamma <- tab_model %>% 
  mutate(o3_mass_conc = o3_mass_conc + 0.01)

v <- 10
r <- 5

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:(v * r), ~sample.int(1000, 2))
seeds[[v * r + 1]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "repeatedcv", 
  number = v, 
  repeats = r,
  verboseIter = TRUE,
  seeds = seeds
)

model <- train(
  x = rec,
  data = tab_model_gamma,
  method = "gam",
  family = Gamma(link = "log"),
  trControl = train_control
)

# model
# summary(model$finalModel)  
# varImp(model)

# Metrics
tab_metrics <- model$results %>% 
  filter(select == model$bestTune$select) %>% 
  extract_gam_metrics(v*r) %>% 
  mutate(link_function = "log")

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/gam_gamma_metrics.rds"
)

# Estimates
tab_estimates <- model$finalModel %>% 
  summary() %>% 
  extract_gam_estimates()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/gam_gamma_estimates.rds"
)

# Smooth term plot

p_gam <- gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "ShareE25",
  ylab = "Effect on the ozone concentration"
)

ggsave(
  "results/dom_pedro_ii/gam_gamma_plot.png",
  plot = p_gam,
  width = 16,
  height = 10
)

# Observed vs predicted plot

p_pred <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("Gamma")

ggsave(
  "results/dom_pedro_ii/gam_gamma_pred_plot.png",
  plot = p_pred,
  width = 16,
  height = 10
)




# Inverse Gaussian fit ----------------------------------------------------

tab_model_inv_gaussian <- tab_model %>% 
  mutate(o3_mass_conc = o3_mass_conc + 0.01)

v <- 10
r <- 5

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:(v * r), ~sample.int(1000, 2))
seeds[[v * r + 1]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "repeatedcv", 
  number = v, 
  repeats = r,
  verboseIter = TRUE,
  seeds = seeds
)

link <- "1/mu^2"

model <- train(
  x = rec,
  data = tab_model_inv_gaussian,
  method = "gam",
  family = inverse.gaussian(link = "1/mu^2"),
  trControl = train_control
)

# model
# summary(model$finalModel)  
# varImp(model)

# Metrics
tab_metrics <- model$results %>% 
  filter(select == model$bestTune$select) %>% 
  extract_gam_metrics(v*r) %>% 
  mutate(link_function = link)

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/gam_inv_gaussian_metrics.rds"
)

# Estimates
tab_estimates <- model$finalModel %>% 
  summary() %>% 
  extract_gam_estimates()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/gam_inv_gaussian_estimates.rds"
)

# Smooth term plot

p_gam <- gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "ShareE25",
  ylab = "Effect on the ozone concentration"
)

ggsave(
  "results/dom_pedro_ii/gam_inv_gaussian_plot.png",
  plot = p_gam,
  width = 16,
  height = 10
)

# Observed vs predicted plot

p_pred <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("Inverse Gaussian")

ggsave(
  "results/dom_pedro_ii/gam_inv_gaussian_pred_plot.png",
  plot = p_pred,
  width = 16,
  height = 10
)

