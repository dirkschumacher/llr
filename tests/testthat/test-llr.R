test_that("some basic calculations", {
  expect_true(llr("(eq 1 1)"))
  expect_false(llr("(eq 1 2)"))
  expect_equal(llr("(+ 1 1)"), 2)
})

test_that("it has first", {
  expect_equal(llr("(first (list 1 2 3))"), 1)
})

test_that("it has rest", {
  expect_equal(llr("(rest (list 1 2 3))"), list(2, 3))
  expect_equal(llr("(rest (list 1))"), NULL)
})

test_that("lambda function", {
  code <- "(map (lambda (x) (+ x 10)) (list 1 2 3))"
  expect_equal(llr(code), list(11, 12, 13))
})

test_that("we support strings", {
  expect_equal(llr("(nchar 'string')"), 6)
})

test_that("logicals are supported", {
  expect_equal(llr("(if TRUE 1 2)"), 1)
})

test_that("exists? supported", {
  expect_true(llr("(exists? 1 (list 1 2 3))"))
  expect_true(llr("(exists? (+ 1 1) (list 1 2 3))"))
  expect_false(llr("(exists? 4 (list 1 2 3))"))
})

test_that("define works", {
  llr("
    (define x 1)
  ")
  expect_equal(get("x"), 1)
})

test_that("functions without arguments", {
  x <- function() 2
  y <- 1
  expect_equal(llr("(y)"), 1)
  expect_equal(llr("(x)"), 2)
})
