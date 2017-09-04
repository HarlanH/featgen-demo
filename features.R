# it's very likely some sort of function-generating function could
# DRY this code some, but keeping it as-is for expository clarity...

# store features -----------

competition_distance <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- generic_feature %>%
    list_modify(name = "CompetitionDistance",
                pretty_name = "Distance to Nearest Competition",
                na = "mean")
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, competition_distance = feat)
  x
}

store_type <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- generic_feature %>%
    list_modify(name = "StoreType",
                pretty_name = "Store Type",
                na = "mode")
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, store_type = feat)
  x
}

# history features --------

current_sales <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- list(
    name="current_sales",
    pretty_name="Current Sales",
    extract = function(self, data, ...) {
      data_frame(current_sales=data$activity[[length(data$activity)]]$Sales)
    }
  )
  
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, current_sales = feat)
  x
}

promo <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- list(
    name="promo",
    pretty_name="Prior Week Sales",
    extract = function(self, data, ...) {
      data_frame(promo=data$activity[[length(data$activity)]]$Promo)
    }
  )
  
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, promo = feat)
  x
}

one_week_ago_sales <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- list(
    name="one_week_ago_sales",
    pretty_name="Prior Week Sales",
    extract = function(self, data, ...) {
      data_frame(one_week_ago_sales=data$activity[[length(data$activity)-7]]$Sales)
    }
  )
  
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, one_week_ago_sales = feat)
  x
}

sales_trend <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- list(
    name="sales_trend",
    pretty_name="Sales Trend",
    days_back=7,
    extract = function(self, data, ...) {
      # extract specific number of days, fit a linear model, and record 
      # the slope
      from = length(data$activity) - self$days_back 
      to = length(data$activity) - 1
      sales <- map_dbl(data$activity[from:to], 
                       ~ .$Sales)
      df <- data_frame(time=seq_len(self$days_back),
                       sales=sales)
      mod <- lm(sales ~ time, df)
      
      data_frame(sales_trend=coef(mod)[['time']])
    }
  )
  
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, sales_trend = feat)
  x
}
