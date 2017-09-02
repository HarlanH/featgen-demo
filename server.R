# curl --data '{"Store": "test!", "CompetitionDistance": 0}' "http://localhost:8000/predict"

#* @get /info
function(req, ...) {
  flog.info("Returning model info")
  
  # glance(model$model) %>%
  #   as.list() 
  model$model$metrics
}

#* @post /predict
function(req, ...){
  obj <- list(...) # should be the same as the objects we trained on
  flog.info(glue("Predicting store: {obj$Store}"))
  
  # extract object into df and predict on that
  predictor_features <- model$features
  predictor_features[[model$target]] <- NULL # drop target
  newdata <- map_dfc(predictor_features, function(feat) {
    flog.trace(glue("Extracting {feat$name}"))
    feat$extract(feat, obj)
  })
  
  flog.debug(paste(names(cars), collapse=", "))
  
  ret <- list(x=predict(model$model, newdata=newdata))
  names(ret) <- model$target
  
  flog.trace("done")
  ret
}
