acme_chain_model("rossman") %>%
  #square_feet(na='mean') %>%
  #store_type(collapse=c("a", "b")) %>%
  competition_distance(trans='boxcox', na='median') %>%
  #one_week_ago_sales %>%
  #sales_weekly_trend(weeks_back=13, trunc_at=c(-.2,.2)) %>%
  #promo %>%
  current_sales %>% 
  get_data %>%
  train(target="current_sales")