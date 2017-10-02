source("../utils.R")
source("../dsl.R")
source("../features.R")

context("features.R")

test_that("competition_distance", {
  x <- competition_distance(acme_chain_model("asdf"), na="min")
  expect_equal(x$features$competition_distance$name, "CompetitionDistance")
  expect_equal(x$features$competition_distance$na, "min")
  
  dat <- list(CompetitionDistance=1:3, asdf=list(zxcv=1, cxvb=3))
  exp <- data_frame(CompetitionDistance=1:3)
  
  expect_equal(x$features$competition_distance$extract(
                  self=x$features$competition_distance,
                  data=dat
                ),
               exp)
})

test_that("store_type", {
  # etc
})

test_that("current_sales", {
  # etc
})

test_that("promo", {
  # etc
})

test_that("one_week_ago_sales", {
  # etc
})

test_that("sales_trend", {
  # etc
})

