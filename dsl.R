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

infer_trans <- function(df, trans) {
  tracethis()
  assert_that(is.data.frame(df))
  assert_that(trans %in% c('log', 'sqrt', 'boxcox'))
  
  if (trans == 'boxcox') {
    map_dfc(df, function(col) {
      boxcoxnc(col, verbose=FALSE, plot=FALSE)$lambda.hat
    })
  } else NULL
}

apply_trans <- function(df, trans, metadata) {
  tracethis()
  assert_that(is.data.frame(df))
  assert_that(trans %in% c('log', 'sqrt', 'boxcox'))
  
  if (trans %in% c('log', 'sqrt')) {
    map_dfc(df, ~ do.call(trans, .))
  } else if (trans == 'boxcox') {
    imap_dfc(df, function(col, pos) {
      if (metadata[[pos]] == 0) 
        log(col)
      else
        (col^metadata[[pos]] - 1) / (metadata[[pos]])
    })
  }
}

#' part 1 of NA/missing data policy
#' 
#' @param df a data frame
#' @param policy one of 'min', 'median', 'mean', 'max', 'mode', or a scalar
#' @return metadata to be used in part 2
infer_missing <- function(df, policy) {
  tracethis()
  assert_that(is.data.frame(df))
  assert_that(length(policy) == 1)
  
  # foreach column, summarize with a replacement value
  map_dfc(df, function(col) {
    if (policy %in% c("min", 'max', 'median', 'mean'))
      do.call(policy, args=list(col, na.rm=TRUE))
    else if (policy == 'mode') {
      val <- names(which.max(table(col, useNA="no")))
      as(val, class(col))
    } else as(policy, class(col))
  })
}

apply_missing <- function(df, metadata) {
  assert_that(are_equal(names(df), names(metadata)))
  
  for (colname in names(df)) {
    missing_elems <- is.na(df[[colname]])
    if (any(missing_elems))
      df[[colname]][missing_elems] <- metadata[[colname]]
  }
  df
}

get_data <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  
  # get the raw data
  raw_data <- readRDS(glue('{x$custname}.Rdata'))
  flog.debug("Got %d data points", length(raw_data))
  
  # foreach feature, foreach data point, build up a data_frame
  x$data <- imap_dfc(x$features, function(feat, pos) {
    flog.trace(glue("Extracting {feat$name}"))
    new_cols <- map_dfr(raw_data, ~ feat$extract(feat, .))
    
    # for both NA and transformations, first infer (possibly a no-op),
    # storing needed info, then apply it. To predict, we'll just apply it.
    if (anyNA(new_cols)) {
      x$features[[pos]]$na_info <<- infer_missing(new_cols, feat$na)
      new_cols <- apply_missing(new_cols, x$features[[pos]]$na_info)
    }
    
    if (!is.null(feat$trans)) {
      x$features[[pos]]$trans_info <<- infer_trans(new_cols, feat$trans)
      new_cols <- apply_trans(new_cols, 
                              feat$trans,
                              x$features[[pos]]$trans_info)
    }
      
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
  x$metrics <- as.list(cv_result$aggr)
  #browser()
  flog.debug("metrics %f", x$metrics)
  
  # build a final model and add to self
  x$target <- args$target
  x$model <- mlr::train(lrn, task)
  
  x
}