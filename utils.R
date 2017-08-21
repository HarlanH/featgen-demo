die <- function(msg) {
  flog.fatal(msg)
  quit(status=1, save="no")
}
