test_that("simple function calls work", {
  res <- llr_test("(r/+ 1 1)")
  expect_equal(res, 2, ignore_attr = TRUE)
  res <- llr_test("(r/sum 1)")
  expect_equal(res, 1, ignore_attr = TRUE)
})
