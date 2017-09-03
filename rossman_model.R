acme_chain_model("rossman") %>%
  #store_type(collapse=c("a", "b")) %>%
  competition_distance(trans='boxcox', na='median') %>%
  one_week_ago_sales(winsor_to=c(2000,15000)) %>%
  #sales_weekly_trend(weeks_back=13, trunc_at=c(-.2,.2)) %>%
  promo %>%
  current_sales %>% 
  get_data %>%
  train(target="current_sales")