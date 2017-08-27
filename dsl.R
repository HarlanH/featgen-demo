acme_chain_model <- function(custname) {
  tracethis()
  obj <- list(custname = custname,
              features = list())
  class(obj) <- "acm"
  obj
}

print.acm <- function(obj, ...) {
  
  cat(glue("
    Acme Chain Model
    ----------------

    Customer = {obj$custname}
    "), "\n\n")
  
  # print features
  if ("features" %in% names(obj)) {
    feature_names <- paste(paste("*", map_chr(obj$features, ~ .$pretty_name)),
                           collapse="\n")
    cat("Features:\n\n")
    cat(feature_names, "\n\n")
  }
  
  # print data summary
  if ("data" %in% names(obj)) {
    cat("Data:\n\n```\n")
    glimpse(obj$data)
    cat("```\n\n")
  }

  # print model summary
  if ("model" %in% names(obj)) {
    cat("Model:\n\n```\n")
    print(summary(obj$model))
    cat("```\n\n")
  }
}

generic_feature <- list(
  name="REPLACEME -- must be the name of the slot",
  pretty_name="Replace Me Too",
  extract = function(self, data, ...) {
    ret=data_frame(x=data[[self$name]])
    names(ret) <- self$name
    ret
  }
)

# this may not be a thing...?
# generic_activity_feature <- list(
#   name="REPLACEME",
#   pretty_name="Replace Me Too",
#   extract = function(self, data, ...) {
#     data$activity[[self$name]]
#   }
# )

competition_distance <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- generic_feature %>%
    list_modify(name = "CompetitionDistance",
                pretty_name = "Distance to Nearest Competition")
  feat <- append(feat, list(...))
  
  x$features <- append(x$features, list(competition_distance = feat))
  x
}

current_sales <- function(x, ...) {
  assert_that(inherits(x, "acm"))
  
  feat <- list(
    name="current_sales",
    pretty_name="Current Sales",
    extract = function(self, data, ...) {
      data_frame(current_sales=data$activity[[length(data$activity)]]$Sales)
    }
  )
  
  feat <- append(feat, list(...))
  
  x$features <- append(x$features, list(current_sales = feat))
  x
}

get_data <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  
  # get the raw data
  raw_data <- readRDS(glue('{x$custname}.Rdata'))
  flog.debug("Got %d data points", length(raw_data))
  
  # foreach feature, foreach data point, build up a data_frame
  x$data <- map_dfc(x$features, function(feat) {
    flog.trace(glue("Extracting {feat$name}"))
    map_dfr(raw_data, ~ feat$extract(feat, .))
  })
  flog.debug("Loaded into %d X %d data_frame", nrow(x$data), ncol(x$data))
  
  x
}

train <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  assert_that(x %has_name% "features")
  assert_that(x %has_name% "data")
  
  # cross-validate to get metrics, then store
  
  # build a final model and add to self
  x$model <- lm(current_sales ~ ., x$data)
  
  x
}