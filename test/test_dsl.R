source("../utils.R")
source("../dsl.R")

context("dsl.R")

test_that("acme_chain_model", {
  expect_s3_class(acme_chain_model("asdf"), "acm")
  expect_equal(acme_chain_model("asdf")$custname, "asdf")
})

test_that("generic_feature", {
  gf <- list_modify(generic_feature, name="speed")
  expect_equal(gf$name, "speed")
  expect_equal(gf$extract(gf, cars)$speed, cars$speed)
})

test_that("log trans", {
  expect_null(infer_trans(cars, "log"))
  expect_equal(apply_trans(cars, "log", NULL)$speed, log(cars$speed))
})

test_that("box-cox trans", {
  bcmd <- infer_trans(cars, "boxcox")
  expect_equal(bcmd$speed, 1.05, tolerance=.01)
  expect_equal(apply_trans(cars, "boxcox", bcmd)$speed[[1]], 3.1, tolerance=.1)
})

test_that("missing data", {
  cars2 <- cars
  cars2$speed[[1]] <- NA
  expect_equal(infer_missing(cars2, "min"), data_frame(speed=4, dist=2))
  expect_equal(infer_missing(cars2, "mode"), data_frame(speed=20, dist=26))
  cars2_md <- infer_missing(cars2, "mode")
  expect_equal(apply_missing(cars2, cars2_md)$speed[[1]], 20)
})

# TODO: break out get_data computation and test!

test_that("train works on cars", {
  # TODO, requires a bit of setup...
})

test_that("predict works on cars", {
  # TODO, requires a bit of setup...
})
