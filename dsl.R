acme_chain_model <- function(custname) {
  obj <- list(custname = custname,
              features = list())
  class(obj) <- "acm"
  obj
}

print.acm <- function(obj, ...) {
  print(glue("
    Acme Chain Model
    ----------------

    Customer = {obj$custname}
            "))
  
  # print features
  # print data summary
  # print model summary
  
}

generic_feature <- list(
  name="REPLACEME",
  pretty_name="Replace Me Too",
  extract = function(self, data, ...) {
    data[[self$name]]
  }
)

generic_activity_feature <- list(
  name="REPLACEME",
  pretty_name="Replace Me Too",
  extract = function(self, data, ...) {
    data$activity[[self$name]]
  }
)

competition_distance <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- generic_feature %>%
    list_modify(name = "competition_distance",
                pretty_name = "Distance to Nearest Competition")
  feat <- append(feat, list(...))
  
  x$features <- append(x$features, list(competition_distance = feat))
  x
}

current_sales <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- generic_activity_feature %>%
    list_modify(name = "current_sales",
                pretty_name = "Current Week Sales")
  feat <- append(feat, list(...))
  
  x$features <- append(x$features, list(current_sales = feat))
  x
}

get_data <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  # foreach feature
    # extract the value and put into a vector
    # do post-processing
  # put results into a data_frame
  x$data = data_frame()
  x
}

train <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  assert_that(x %has_name% "features")
  assert_that(x %has_name% "data")
  
  # cross-validate to get metrics, then store
  
  # build a final model and add to self
  
  x
}