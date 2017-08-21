#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(futile.logger))
source("utils.R")
source("dsl.R")

parser <- ArgumentParser(description='Acme Sales Model')
parser$add_argument("--debug", default="INFO",
                    help="Debug level")
parser$add_argument('varargs', metavar='command', nargs='+',
                    help='train <from> <to>, server <from>, client, report <from> <to>, <test>')

args <- parser$parse_args()

if (args$varargs[[1]] == "train") {
  flog.threshold(args$debug)
  flog.info("Starting train...")
  if (length(args$varargs) != 3) die("Usage: acm.R train infile.R outfile.Rout")
  infilename <- args$varargs[[2]]
  outfilename <- args$varargs[[3]]
  
  # eval the infile, saving the results to outfile
  if (!file.exists(infilename)) die("specified input file doesn't exist")
  model <- eval(parse(file=infilename))
  
  save(model, file=outfilename)
  flog.info("Completed train!")
} else if (args$varargs[[1]] == "server") {
  
} else if (args$varargs[[1]] == "client") {
  
} else if (args$varargs[[1]] == "report") {
  
} else if (args$varargs[[1]] == "test") {
  
} else {
  die(paste("Unknown args", args, collapse=", "))
}
