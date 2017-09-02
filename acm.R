#!/usr/bin/env Rscript

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

source("utils.R")
source("dsl.R")

parser <- ArgumentParser(description='Acme Sales Model')
parser$add_argument("--debug", default="INFO",
                    help="Debug level")
parser$add_argument("--port", default="8000",
                    help="Port for server")
parser$add_argument('varargs', metavar='command', nargs='+',
                    help='train <from> <to>, server <from>, client, report <from> <to>, <test>')

args <- if (interactive()) {
  parser$parse_args(c("train", "rossman_model.R", "rossman_model.Rout", "--debug=TRACE"))
} else {
  parser$parse_args()
}

if (args$varargs[[1]] == "train") { # train --------
  flog.threshold(args$debug)
  flog.info("Starting train...")
  if (length(args$varargs) != 3) die("Usage: acm.R train infile.R outfile.Rout")
  infilename <- args$varargs[[2]]
  outfilename <- args$varargs[[3]]
  
  # eval the infile, saving the results to outfile
  if (!file.exists(infilename)) die("specified input file doesn't exist")
  model <- eval(parse(file=infilename))
  
  saveRDS(model, file=outfilename)
  flog.info("Completed train!")
  
} else if (args$varargs[[1]] == "server") { # server -----------
  flog.threshold(args$debug)
  flog.info("Starting server...")
  if (length(args$varargs) != 2) die("Usage: acm.R server infile.R")
  suppressPackageStartupMessages(library(plumber))
  
  infilename <- args$varargs[[2]]
  model <- readRDS(infilename) # NB: we're in the global environment!
  flog.info("Model loaded")
  
  server <- plumb("server.R")
  flog.info("Starting server...")
  server$run(port=as.numeric(args$port))
  flog.info("Closing...")
  
} else if (args$varargs[[1]] == "client") { # client -------
  flog.info("Starting client...")
  shiny::runApp("client")
  flog.info("Closing...")
  
} else if (args$varargs[[1]] == "print") { # print ---------
  if (length(args$varargs) != 2) die("Usage: acm.R print infile.R")
  infilename <- args$varargs[[2]]
  model <- readRDS(infilename)
  
  print(model)
  
} else if (args$varargs[[1]] == "report") { # report -------
  
} else if (args$varargs[[1]] == "test") { # test --------
  suppressPackageStartupMessages(library(testthat))
  test_dir("test")
} else {
  die(paste("Unknown args", args, collapse=", "))
}
