die <- function(msg) {
  flog.fatal(msg)
  quit(status=1, save="no")
}

tracethis <- function() {
  flog.trace(deparse(sys.call(-1)))
}