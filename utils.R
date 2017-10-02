options(stringsAsFactors = FALSE,
        warnPartialMatchArgs = TRUE,
        warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE)

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(futile.logger))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(mlr))
suppressPackageStartupMessages(library(AID))
suppressPackageStartupMessages(library(forcats))

#' Log a fatal error and quit.
#' 
#' @param msg a string to log
#' @return doesn't
die <- function(msg) {
  flog.fatal(msg)
  quit(status=1, save="no")
}

#' Log the name of the calling function!
tracethis <- function() {
  flog.trace(deparse(sys.call(-1)))
}