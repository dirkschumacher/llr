test_that("simple map works", {
  res <- llr_test("
    {:a 10 :b 20}
  ")
  expect_equal(res$get(":a"), 10, ignore_attr = TRUE)
  expect_equal(res$get(":b"), 20, ignore_attr = TRUE)
})

test_that("any key works", {
  res <- llr_test('
(def some-map {:a 1 :b 41 r/datasets::mtcars "woot"})
(def res
  (+ (get some-map :a) (get some-map :b)))
(paste res (get some-map r/datasets::mtcars))
')
  expect_equal(res, "42 woot", ignore_attr = TRUE)
})

test_that("dynamic keys", {
  res <- llr_test("
  (def w 1)
  {w 1}
  ")
  expect_equal(res$length(), 1)
  expect_equal(res$keys()[[1]], 1, ignore_attr = TRUE)
  expect_equal(res$values()[[1]], 1, ignore_attr = TRUE)
})
