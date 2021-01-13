test_that("ignore next form", {
  res <- llr_test("#_{:a 1 :b 2 :b 4} 42")
  expect_equal(res, 42, ignore_attr = TRUE)
})
