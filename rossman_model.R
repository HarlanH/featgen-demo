acme_chain_model("rossman") %>%
  store_type(collapse=list(ab=c("a", "b"))) %>%
  competition_distance(trans='boxcox', na='median') %>%
  one_week_ago_sales(winsor_to=c(2000,15000)) %>%
  sales_trend(days_back=4*7, trunc_at=c(-100, 100), na='mean') %>%
  promo %>%
  current_sales %>% 
  get_data %>%
  train(target="current_sales")