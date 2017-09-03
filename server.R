# curl --data '{"Store": "test!", "CompetitionDistance": 0}' "http://localhost:8000/predict"

#* @get /info
function(req, ...) {
  flog.info("Returning model info")
  
  tidy(model$model$learner.model) %>% 
    select(thing=term, value=estimate) %>%
    bind_rows(data_frame(thing="MSE", value=model$metrics[[1]]))
}

#* @post /predict
function(req, ...){
  obj <- list(...) # should be the same as the objects we trained on
  flog.info(glue("Predicting store: {obj$Store}"))
  
  # extract object into df and predict on that
  predictor_features <- model$features
  predictor_features[[model$target]] <- NULL # drop target
  
  # TODO: pull this into dsl.R?
  newdata <- imap_dfc(predictor_features, function(feat, pos) {
    flog.trace(glue("Extracting {feat$name}"))
    new_cols <- feat$extract(feat, obj)
    
    if (anyNA(new_cols)) {
      new_cols <- apply_missing(new_cols, model$features[[pos]]$na_info)
    }
    
    if (!is.null(feat$trans)) {
      new_cols <- apply_trans(new_cols, 
                              feat$trans,
                              model$features[[pos]]$trans_info)
    }
    new_cols
  })
  
  ret <- list(x=predict(model$model, newdata=newdata)$data$response)
  names(ret) <- model$target

  flog.trace("done")
  ret
}
