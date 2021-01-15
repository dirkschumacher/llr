test_that("threading macro", {
  res <- llr_test("
  (->
    (r/seq 1 10)
    (r/lapply (fn [x] (* x x)))
    (r/purrr::keep (fn [x] (= x 25))))
  ")
  expect_equal(res, list(25), ignore_attr = TRUE)
})

test_that("addition uses sum for variadic functions", {
  res <- llr_test("(+ 1 2 3 4 5 6 7 8 9 10)")
  expect_equal(res, sum(1:10), ignore_attr = TRUE)
})

test_that("comp works", {
  res <- llr_test("((comp inc inc inc) 0)")
  expect_equal(res, 3, ignore_attr = TRUE)
  res <- llr_test("((comp inc) 0)")
  expect_equal(res, 1, ignore_attr = TRUE)
})
