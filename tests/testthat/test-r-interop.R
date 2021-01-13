test_that("r/ prefix #1", {
  res <- llr_test("(r/rnorm 10)")
  expect_equal(length(res), 10)
  expect_true(is.numeric(res))
})

test_that("r/ package call", {
  res <- llr_test("(r/base::abs -10)")
  expect_equal(res, 10)
})

test_that("r/ named parameters", {
  res <- llr_test("(r/round :x 10.1234 :digits 2)")
  expect_equal(res, round(x = 10.1234, digits = 2L), ignore_attr = TRUE)
})

test_that("r/built in stuff", {
  expect_equal(llr_test("r/mtcars"), mtcars)
})

test_that("r dplyr works", {
  res <- llr_test("
  (r/suppressPackageStartupMessages (r/library dplyr :quietly true))
  (->
    r/datasets::mtcars
    (r/dplyr::filter (> hp 100)) ; filter is in std.lib
    (r/summarise :count (n) :mean_mpg (mean mpg)))
  ")
  expect_true(is.data.frame(res))
})
