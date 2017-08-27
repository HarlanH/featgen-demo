
#* @post /predict
function(req, ...){
  obj <- list(...)
  # TODO: extract object into df and predict on that
  ret <- list(x=predict(model$model, newdata=obj))
  names(ret) <- model$target
  ret
}