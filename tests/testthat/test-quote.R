test_that("quote list and vectors #1", {
  expect_silent(res <- llr_test("'(1 2 a)"))
  expect_equal(res, ral_list(1L, 2L, sym("a")), ignore_attr = TRUE)
  expect_silent(res <- llr_test("(quote (1 2 a))"))
  expect_equal(res, ral_list(1L, 2L, sym("a")), ignore_attr = TRUE)
})

test_that("syntax quote edge cases", {
  expect_equal(as.integer(llr_test("`1")), 1L)
  expect_equal(as.integer(llr_test("`~1")), 1L)
  expect_equal(as.integer(llr_test("`~1")), 1L)
  expect_equal(as.integer(llr_test("`~(+ 1 1)")), 2L)
})

test_that("syntax quote UQ works", {
  res <- llr_test("
    (let [a 1 b 2] `(+ ~a ~b))
  ")
  expect_equal(unlist(res), list(sym("+"), 1L, 2L))
})

test_that("unquote splicing works", {
  res <- llr_test("`(1 ~@[1 2 3])")
  expect_equal(unlist(res), c(1L, 1L, 2L, 3L), ignore_attr = TRUE)
})

test_that("UQ on maps", {
  res <- llr_test("`{:a ~(+ 1 1)}")
  expect_equal(res$get(":a"), 2, ignore_attr = TRUE)
  res <- llr_test("(defn w [] :a) `{~(w) ~(+ 1 1)}")
  expect_equal(res$get(":a"), 2, ignore_attr = TRUE)
})

test_that("UQ on maps2", {
  res <- llr_test("
  (def dynamic_symbol (fn [] :a))
  (get `{~(dynamic_symbol) ~(+ 1 1)} :a)
  ")
  expect_equal(res, 2, ignore_attr = TRUE)
})
