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
                pretty_name = "Distance to Nearest Competition",
                na = "mean")
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, competition_distance = feat)
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
  
  feat <- list_modify(feat, ...)
  
  x$features <- list_modify(x$features, current_sales = feat)
  x
}

#' processing NA/missing data policy
#' 
#' Note: casts all columns to numeric/doubles!
#' 
#' @param df a data frame
#' @param policy one of 'min', 'median', 'mean', 'max', 'mode', or a scalar
#' @return data frame of same shape as df, with no NAs
process_missing <- function(df, policy) {
  tracethis()
  assert_that(is.data.frame(df))
  assert_that(length(policy) == 1)
  
  # this doesn't yet handle non-numeric types...
  ret <- map_dfc(df, function(col) {
    col <- as.numeric(col)
    rep_val <- case_when(
      policy == 'min' ~ min(col, na.rm = TRUE),
      policy == 'max' ~ max(col, na.rm = TRUE),
      policy == 'median' ~ median(col, na.rm = TRUE),
      policy == 'mean' ~ mean(col, na.rm = TRUE),
      policy == 'mode' ~ as.numeric(names(which.max(table(col, useNA="no")))),
      TRUE ~ as.numeric(policy)
    )
    col[is.na(col)] <- rep_val
    col
  })
  assert_that(!anyNA(ret))
  ret
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
    new_cols <- map_dfr(raw_data, ~ feat$extract(feat, .))
    new_cols <- process_missing(new_cols, feat$na)
    new_cols
  })
  flog.debug("Loaded into %d X %d data_frame", nrow(x$data), ncol(x$data))
  
  x
}

train <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  assert_that(x %has_name% "features")
  assert_that(x %has_name% "data")
  args <- list(...)
  assert_that(args %has_name% "target")
  
  # set up learning framework
  flog.debug("setting up learning framework")
  task <- makeRegrTask(data = x$data, target = args$target)
  lrn <- makeLearner("regr.lm")
  
  # cross-validate to get metrics, then store
  cv_result <- crossval(lrn, task, iters = 10, keep.pred=FALSE)
  x$metrics <- cv_result$aggr
  
  # build a final model and add to self
  x$target <- args$target
  x$model <- mlr::train(lrn, task)
  
  x
}