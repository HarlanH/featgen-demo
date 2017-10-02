# core DSL ---------

#' Object constructor for a specific acm model
#' 
#' @param custname string, name of the customer; must be name of data file
#' @return an object of class "acm"
acme_chain_model <- function(custname) {
  tracethis()
  obj <- list(custname = custname,
              features = list())
  class(obj) <- "acm"
  obj
}

#' Print method for ACM objects.
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

# Feature utilities ----------

#' Helper template for top-level simple features.
#' Use `list_modify` to customize.
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

# Transformations --------

#' Perform any computation needed on data before transformation.
#' 
#' Results will be stored as metadata for `apply_trans`.
#' 
#' Currently stores the lambda value for Box-Cox transformation,
#' and does no-op for log and sqrt transformations.
#' 
#' @param df a data frame with columns to process
#' @param trans one of "log", "sqrt", "boxcox"
#' @return a data frame with the same columns as df, and one row, or NULL
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

#' Transform the specified data frame, leveraging earlier metadata
#' 
#' @param df a data frame
#' @param trans one of "log", "sqrt", "boxcox"
#' @param metadata from `infer_trans`
#' @return a data frame the same size as df
apply_trans <- function(df, trans, metadata) {
  tracethis()
  assert_that(is.data.frame(df))
  assert_that(trans %in% c('log', 'sqrt', 'boxcox'))
  
  if (trans %in% c('log', 'sqrt')) {
    map_dfc(df, ~ do.call(trans, list(.)))
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
      methods::as(val, class(col))  # fails in test without package name??
    } else methods::as(policy, class(col))
  })
}

#' part 2 of NA/missing data policy
#' 
#' @param df a data frame
#' @param metadata from `infer_missing`
#' @return data frame with no missing data
apply_missing <- function(df, metadata) {
  assert_that(are_equal(names(df), names(metadata)))
  
  for (colname in names(df)) {
    missing_elems <- is.na(df[[colname]])
    if (any(missing_elems))
      df[[colname]][missing_elems] <- metadata[[colname]]
  }
  df
}

# Fixed DSL verbs ------

#' Load data and process into data frame for training
#' 
#' @param x an ACM object with `custname` slot as name of data file
#' @param ... not currently used?
#' @return object with new `data` slot
get_data <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  
  # get the raw data
  raw_data <- readRDS(glue('{x$custname}.Rdata'))
  flog.debug("Got %d data points", length(raw_data))
  
  # foreach feature, foreach data point, build up a data_frame
  x$data <- imap_dfc(x$features, function(feat, pos) {
    flog.debug(glue("Extracting {feat$name}"))
    new_cols <- map_dfr(raw_data, ~ feat$extract(feat, .))
    
    # mlr is going to want either numeric (incl logical) or factors. Make
    # sure this is the case. Also, do factor collapsing.
    for (colidx in seq_along(new_cols)) {
      if (is.character(new_cols[[colidx]])) {
        new_cols[[colidx]] <- as.factor(new_cols[[colidx]])
      }
      
      if (is.factor(new_cols[[colidx]]) && !is.null(feat$collapse)) {
        new_cols[[colidx]] <- do.call(fct_collapse, 
                                      append(list(f=new_cols[[colidx]]), 
                                             feat$collapse))
      }
    }
    
    # trim to null, cap at value, windsorize at percent
    if (!is.null(feat$trim_to)) {
      assert_that(length(feat$trim_to) == 2)
      flog.debug("Trimming to NA outside of %0.2f and %0.2f", 
                 feat$trim_to[[1]], feat$trim_to[[2]])
      new_cols <- map_dfc(new_cols, function(col) {
        ifelse(between(col, feat$trim_to[[1]], feat$trim_to[[2]]),
               col,
               NA)
      })
    }
    
    if (!is.null(feat$cap_to)) {
      assert_that(length(feat$cap_to) == 2)
      flog.debug("Capping outside of %0.2f and %0.2f", 
                 feat$cap_to[[1]], feat$cap_to[[2]])
      new_cols <- map_dfc(new_cols, function(col) {
        pmax(pmin(col, feat$cap_to[[2]]),
             feat$cap_to[[1]])
      })
    }
    
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

#' Train a model
#' 
#' @param x an ACM object with `features`, `data`
#' @param ... parameters; must include `target`
#' @return object with new `cv_preds`, `metrics`, `target`, and `model` slots
train <- function(x, ...) {
  tracethis()
  assert_that(inherits(x, "acm"))
  assert_that(x %has_name% "features")
  assert_that(x %has_name% "data")
  args <- list(...)
  assert_that(args %has_name% "target") # TODO: make a real param!
  
  # set up learning framework
  flog.debug("setting up learning framework")
  task <- makeRegrTask(data = x$data, target = args$target)
  lrn <- makeLearner("regr.lm")
  
  # cross-validate to get metrics, then store
  cv_result <- crossval(lrn, task, iters = 10)
  x$cv_preds <- getRRPredictions(cv_result)$data
  x$metrics <- as.list(cv_result$aggr)
  flog.debug("metrics %f", x$metrics)
  
  # build a final model and add to self
  x$target <- args$target
  x$model <- mlr::train(lrn, task)
  
  x
}

#' Predict on a JSON object
#' 
#' @param x an ACM object with `features` and `model` and `target`
#' @param obj a JSON-structure list object
#' @return a list with one value, named after the target
predict.acm <- function(x, obj) {
  tracethis()
  assert_that(inherits(x, "acm"))
  assert_that(x %has_name% "features")
  assert_that(x %has_name% "target")
  assert_that(x %has_name% "model")
  
  # extract object into df and predict on that
  predictor_features <- x$features
  predictor_features[[x$target]] <- NULL # drop target
  
  newdata <- imap_dfc(predictor_features, function(feat, pos) {
    flog.trace(glue("Extracting {feat$name}"))
    new_cols <- feat$extract(feat, obj)
    
    if (anyNA(new_cols)) {
      new_cols <- apply_missing(new_cols, x$features[[pos]]$na_info)
    }
    
    if (!is.null(feat$trans)) {
      new_cols <- apply_trans(new_cols, 
                              feat$trans,
                              x$features[[pos]]$trans_info)
    }
    new_cols
  })
  
  ret <- list(x=predict(x$model, newdata=newdata)$data$response)
  names(ret) <- x$target
  
  ret
}
