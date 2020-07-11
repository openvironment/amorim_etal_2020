default_color <- "royal blue"

format_table <- function(tab, cap = NULL) {
  tab %>% 
    knitr::kable(caption = cap)
}

estimates_table <- function(tab) {
  tab %>% 
    arrange(p.value) %>% 
    mutate_if(is.numeric, ~round(., 2)) %>%
    format_table()
}

hp_table <- function(tab, model_name) {
  tab %>% 
    filter(model == model_name) %>% 
    distinct(model, .keep_all = TRUE) %>% 
    select(mtry:sample_size) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(!is.na(value)) %>% 
    format_table(cap = "hyperparameters")
}

estimates_plot <- function(tab, metric, rev = TRUE) {
  
  lm_limits <- tab %>% 
    filter(model == "linear_regression", .metric == metric) %>% 
    mutate(
      li = mean - 2 * std_err,
      ls = mean + 2 * std_err
    ) %>% 
    select(li, ls)
  
  tab %>% 
    filter(.metric == metric) %>% 
    mutate(
      li = mean - 2 * std_err, 
      ls = mean + 2 * std_err,
      model = forcats::fct_reorder(model, mean, .desc = rev)
    ) %>% 
    ggplot(aes(x = mean, y = model, color = model)) +
    geom_vline(xintercept = lm_limits$li, linetype = 2, size = 0.2) +
    geom_vline(xintercept = lm_limits$ls, linetype = 2, size = 0.2) +
    geom_point(size = 2, show.legend = FALSE) +
    geom_errorbar(
      aes(xmin = li, xmax = ls), 
      width = 0, 
      size = 1.2, 
      show.legend = FALSE
    ) +
    labs(x = metric)
}