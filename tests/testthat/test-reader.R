test_that("use the reader in code", {
  res <- llr_test('
    (def inception (read-string "(+ 32 10)"))
    inception
    (eval inception)
  ')
  expect_equal(res, 42, ignore_attr = TRUE)
})

test_that("errors if missing paren", {
  expect_error(
    llr_test("(if 1 (r/pi)")
  )
})
