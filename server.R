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
  
  ret <- predict(model, obj) # model is a global!

  flog.trace("done")
  ret
}
