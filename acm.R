#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(argparse))

parser <- ArgumentParser(description='Acme Sales Model')
parser$add_argument("--debug", default="INFO",
                    help="Debug level")
parser$add_argument('varargs', metavar='command', nargs='+',
                    help='train <from> <to>, server <from>, client, report <from> <to>, <test>')

args <- parser$parse_args()

if (args[[1]] == "train") {
  
} else if (args[[1]] == "server") {
  
} else if (args[[1]] == "client") {
  
} else if (args[[1]] == "report") {
  
} else if (args[[1]] == "test") {
  
}
