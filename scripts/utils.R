
# Tables ------------------------------------------------------------------

coef_table <- function(fit) {
  
  stations <- map(fit, ~.$station) %>% 
    flatten_chr()
  
  map(fit, ~.$coef) %>% 
    transpose() %>%
    map(flatten_dbl) %>% 
    as_tibble() %>% 
    add_column(Station = stations, .before = 1) %>% 
    mutate_if(is.numeric, funs(round), digits = 2)
}


rmse_table <- function(fit) {
  
  stations <- map(fit, ~.$station) %>% 
    flatten_chr()
  
  map(fit, ~.$rmse) %>% 
    flatten_dbl() %>% 
    set_names(stations) %>%
    data.frame(RMSE = .) %>% 
    tibble::rownames_to_column(var = "Station") %>% 
    mutate_if(is.numeric, funs(round), digits = 2)
}


# Plots -------------------------------------------------------------------s

pred_obs_plot2 <- function(df, model, y) {
  
  df %>% 
    na.omit() %>% 
    mutate(pred = predict(model, na.omit(df))) %>% 
    select(y = y, pred) %>%
    ggplot(aes(x = y, y = pred)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(x = "Valores observados", y = "Valores preditos") +
    theme_bw()
  
}


