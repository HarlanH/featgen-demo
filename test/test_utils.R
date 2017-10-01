source("../utils.R")

context("utils.R")

test_that("tracethis", {
  flog.threshold("TRACE")
  asdf <- function() {
    tracethis()
    42
  }
  expect_output(asdf(), "TRACE .* asdf")
})